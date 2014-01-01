{-| Use the 'Model' - 'View' - 'Controller' pattern to separate concurrency from
    application logic.

-}

{-# LANGUAGE RankNTypes #-}

module MVC (
    -- * Controller
    -- $controller
      Controller(..)
    , once
    , fromProducer

    -- * View
    -- $view
    , View(..)
    , Handler(..)
    , handling
    , (<#>)
    , fromHandler

    -- * Model
    -- $model
    , Model
    , runMVC

    -- * Managed Resources
    , Managed
    , with
    , manage

    -- * Re-exports
    -- $reexports
    , module Control.Monad.Trans.State.Strict
    , module Data.Functor.Constant
    , module Data.Monoid
    , module Pipes
    , (<$>)
    ) where

import Control.Applicative (
    Applicative(pure, (<*>)), Alternative(empty, (<|>)), liftA2, (<*), (<$>))
import Control.Concurrent.Async (withAsync)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.State.Strict
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Monoid (
    Monoid(mempty, mappend, mconcat), (<>), First(First, getFirst) )
import Pipes
import Pipes.Concurrent

{- $controller
    'Controller's represent concurrent inputs to your system.  Use the 'Functor'
    and 'Monoid' instance of 'Controller' to bundle multiple 'Controller's
    together:

> controllerA :: Controller A
> controllerB :: Controller B
>
> controllerTotal :: Controller (Either A B)
> controllerTotal = fmap Left controllerA <> fmap Right controllerB
-}

-- | A 'Controller' is an 'Input' bundled with resource management logic
newtype Controller a = Controller { runController :: Managed (Input a) }

instance Functor Controller where
    fmap f (Controller x) = Controller (fmap (fmap f) x)

instance Applicative Controller where
    pure a    = Controller (pure (pure a))
    mf <*> mx = Controller $
        liftA2 (<*>) (runController mf) (runController mx)

instance Alternative Controller where
    empty = mempty
    (<|>) = mappend

instance Monoid (Controller a) where
    mempty = Controller (pure mempty)
    mappend (Controller x) (Controller y) = Controller (liftA2 mappend x y)

-- | Create a 'Controller' that emits a single value
once :: Controller ()
once = fromProducer Single (yield ())
{-# INLINABLE once #-}

-- | Create a 'Controller' from a 'Producer'
fromProducer :: Buffer a -> Producer a IO () -> Controller a
fromProducer buffer producer = Controller $ manage $ \k -> do
    (output, input, seal) <- spawn' buffer
    let io = do
            runEffect $ producer >-> toOutput output
            seal
    withAsync io $ \_ -> k input <* seal
{-# INLINABLE fromProducer #-}

{- $view
    'View's represent outputs of your system.  Use the 'Handler' and 'Monoid'
    instances of 'View' to combine multiple 'View's together into a single
    'View' using prisms from the @lens@ library:

> import Control.Lens (_Left, _Right)
>
> viewA :: View A
> viewB :: View B
>
> viewTotal :: View (Either A B)
> viewTotal = handling _Left viewA <> handling _Right viewB

    Use 'Control.Lens.makePrisms' from the @lens@ library to auto-generate
    prisms for your own output event streams.
-}

-- | A 'View' is an 'Output' bundled with resource management logic
newtype View a = View { runView :: Managed (Output a) }

instance Monoid (View a) where
    mempty = View (pure mempty)
    mappend (View x) (View y) = View (liftA2 mappend x y)

{-| A contravariant functor that transforms 'Maybe' Kleisli arrows to
    functions between handlers

    All instances must satisfy the following laws:

> handle return = id
> handle (f >=> g) = handle f . handle g
-}
class Handler f where
    -- | Pre-map a partial getter to define a partial handler
    handle :: (a -> Maybe b) -> f b -> f a

instance Handler Output where
    handle f o = Output $ \a ->
        case f a of
            Nothing -> return True
            Just b  -> send o b

instance Handler View where
    handle f v = View (fmap (handle f) (runView v))

-- | Create a 'View' from a handler
fromHandler :: (a -> IO Bool) -> View a
fromHandler handler = View (pure (Output handler))
{-# INLINABLE fromHandler #-}

{-| This is a variation on 'handle' designed to work with prisms auto-generated
    by the @lens@ library.  Think of the type as:

> handling :: (Handles f) => Prism' a b -> f b -> f a

    @(handling prism action)@ only runs the @action@ if the @prism@ matches the
    @action@'s input, using the prism to transform the input.

> handling id = id
>
> handling (p1 . p2) = handling p1 . handling p2
-}
handling
    :: (Handler f)
    => ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> (f b -> f a)
    -- ^
handling k = handle (getFirst . getConstant . k (Constant . First . Just))
{-# INLINABLE handling #-}

-- | An infix synonym for 'handling'
(<#>)
    :: (Handler f)
    => ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> (f b -> f a)
    -- ^
(<#>) = handling
{-# INLINABLE (<#>) #-}

infixr 7 <#>

{- $model
    'Model's are 'ListT' streams enriched with global state and sit in between
    'Controller's and 'View's.  Connect a 'Model', 'View', and 'Controller'
    together using 'runMVC' to complete your application.

    The 'Model' is designed to be entirely pure and concurrency-free so that you
    can @QuickCheck@ it, equationally reason about its behavior, debug it
    deterministically, or save and replay old event streams as test cases.
-}

{-| A @(Model s a b)@ converts a stream of @(a)@s a stream of @(b)@s while
    interacting with state @(s)@
-}
type Model s a b = a -> ListT (State s) b

{-| Connect a 'Model', 'View', and 'Controller' into a complete application by
    providing an initial state
-}
runMVC :: Controller a -> Model s a b -> View b -> s -> IO ()
runMVC controller model view initialState =
    with (runController controller) $ \input  ->
    with (runView       view      ) $ \output ->
    flip evalStateT initialState $ runEffect $
        fromInput input >-> pipe >-> toOutput output
  where
    pipe = for cat $ \a -> hoist (hoist generalize) (every (model a))
{-# INLINABLE runMVC #-}

-- | A @(Managed r)@ is a resource @(r)@ bracketed by acquisition and release
newtype Managed r = Manage
    { -- | Consume a managed resource
      with :: forall x . (r -> IO x) -> IO x
    }

-- | Build a 'Managed' resource
manage :: (forall x . (r -> IO x) -> IO x) -> Managed r
manage = Manage
{-# INLINABLE manage #-}

instance Functor Managed where
    fmap f m = Manage (\k -> with m (\r -> k (f r)))

instance Applicative Managed where
    pure a    = Manage (\k -> k a)
    mf <*> mx = Manage (\k -> with mf (\f -> with mx (\x -> k (f x))))

instance Monad Managed where
    return a = Manage (\k -> k a)
    m >>= f  = Manage (\k -> with m (\a -> with (f a) k))

{- $reexports
    @Data.Functor.Constant@ re-exports 'Constant' (the type only)

    @Data.Monoid@ re-exports 'Monoid', ('<>'), 'mconcat', and 'First'

    All other modules re-export everything
-}
