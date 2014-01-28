{-| Use the 'Model' - 'View' - 'Controller' pattern to separate concurrency from
    application logic.
-}

{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module MVC (
    -- * Controllers
    -- $controller
      Controller
    , (<$>)
    , (<$)
    , fromProducer

    -- * Views
    -- $view
    , View(..)
    , handles
    , handling
    , (<#>)

    -- * Models
    -- $model
    , Model
    , runMVC
    , fromListT
    , L.zoom
    , readOnly
    , generalize

    -- * Managed Resources
    , Managed
    , with
    , manage

    -- * Re-exports
    -- $reexports
    , module Control.Monad.Trans.Reader
    , module Control.Monad.Trans.State.Strict
    , module Data.Functor.Constant
    , module Data.Monoid
    , module Lens.Family
    , module Lens.Family.State.Strict
    , module Pipes
    , module Pipes.Concurrent
    ) where

import Control.Applicative (Applicative(pure, (<*>)), (<*), (<$>), (<$))
import Control.Concurrent.Async (withAsync)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.State.Strict (StateT, State, get, put, modify)
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Reader (ReaderT, Reader, ask)
import qualified Control.Monad.Trans.Reader as R
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>), First)
import qualified Data.Monoid as M
import Lens.Family (LensLike')
import Lens.Family.State.Strict (Zooming)
import qualified Lens.Family.State.Strict as L
import Pipes
import Pipes.Concurrent

{- $controller
    'Controller's represent concurrent inputs to your system.  Use the 'Functor'
    and 'Monoid' instances of 'Controller' to bundle multiple 'Controller's
    together:

> controllerA     :: Controller A             -- A source of 'A's
> controllerB     :: Controller B             -- A source of 'B's
>
> controllerTotal :: Controller (Either A B)  -- A source of either 'A's or 'B's
> controllerTotal = fmap Left controllerA <> fmap Right controllerB
>
> -- Alternatively:
> --
> -- controllerTotal = mconcat
> --     [ Left  <$> controllerA
> --     , Right <$> controllerB
> --     ]
-}

-- | A 'Controller' is a synonym for an 'Input' from @pipes-concurrency@
type Controller = Input

-- | Create a 'Managed' 'Controller' from a 'Producer'
fromProducer :: Buffer a -> Producer a IO () -> Managed (Controller a)
fromProducer buffer producer =
    manage $ \k -> do
        (output, input, seal) <- spawn' buffer
        let io = do
                runEffect $ producer >-> toOutput output
                atomically seal
        withAsync io $ \_ -> k input <* atomically seal
{-# INLINABLE fromProducer #-}

{- $view
    'View's represent outputs of your system.  Use 'handling' and the 'Monoid'
    instance of 'View' to combine multiple 'View's together into a single 'View'
    using prisms from the @lens@ library:

> import Control.Lens (_Left, _Right)
>
> viewA     :: View A             -- A sink of 'B's
> viewB     :: View B             -- A sink of 'A's
>
> viewTotal :: View (Either A B)  -- A sink of either 'A's or 'B's
> viewTotal = handling _Left viewA <> handling _Right viewB
>
> -- Alternatively:
> --
> -- viewTotal = mconcat
> --     [ _Left  <#> viewA
> --     , _Right <#> viewB
> --     ]

    Use 'Control.Lens.makePrisms' from the @lens@ library to auto-generate
    prisms for your own output event streams.

    If a `lens` dependency is too heavy-weight, then use 'handles' instead.
-}

{-| A 'View' is an 'IO'-based handler

    'View's resemble 'Output's from @pipes-concurrency@, except that they run in
    'IO' instead of 'STM'
-}
newtype View a = View { sendIO :: a -> IO () }

instance Monoid (View a) where
    mempty = View (\_ -> return ())
    mappend v1 v2 = View (\a -> sendIO v1 a >> sendIO v2 a)

{-| Pre-map a partial getter to define a partial handler

> handles return = id
>
> handles (f <=< g) = handles g . handles f
-}
handles :: (a -> Maybe b) -> View b -> View a
handles f o = View $ \a -> case f a of
    Nothing -> return ()
    Just b  -> sendIO o b

{-| This is a variation on 'handles' designed to work with prisms auto-generated
    by the @lens@ library.  Think of the type as:

> handling :: APrism' a b -> View b -> View a

    @(handling prism action)@ only runs the @action@ if the @prism@ matches the
    @action@'s input, using the prism to transform the input.

> handling id = id
>
> handling (p1 . p2) = handling p2 . handling p1
-}
handling
    :: ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> (View b -> View a)
    -- ^
handling k = handles (M.getFirst . getConstant . k (Constant . M.First . Just))
{-# INLINABLE handling #-}

-- | An infix synonym for 'handling'
(<#>)
    :: ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> (View b -> View a)
    -- ^
(<#>) = handling
{-# INLINABLE (<#>) #-}

infixr 7 <#>

{- $model
    'Model's are stateful streams and they sit in between 'Controller's and
    'View's.  Connect a 'Model', 'View', and 'Controller' together using
    'runMVC' to complete your application.

    The 'Model' is designed to be pure and concurrency-free so that you can:

    * @QuickCheck@ your model,

    * equationally reason about its behavior, and:

    * replay saved event streams deterministically.
-}

{-| A @(Model s a b)@ converts a stream of @(a)@s into a stream of @(b)@s while
    interacting with a state @(s)@
-}
type Model s a b = Pipe a b (State s) ()

-- | Connect a 'Model', 'View', and 'Controller' into a complete application.
runMVC
    :: Controller a
    -- ^ Effectful input
    -> Model s a b
    -- ^ Program logic
    -> View b
    -- ^ Effectful output
    -> s
    -- ^ Initial state
    -> IO ()
runMVC controller model view initialState =
    flip S.evalStateT initialState $ runEffect $
            fromInput controller
        >-> hoist (hoist generalize) model
        >-> for cat (liftIO . sendIO view)
{-# INLINABLE runMVC #-}

{-| Convert a 'ListT' transformation to a 'Model'

> fromListT :: (a -> ListT (State s) b) -> Model s a b

> fromListT (k1 >=> k2) = fromListT k1 >-> fromListT k2
>
> fromListT return = cat
-}
fromListT :: (Monad m) => (a -> ListT m b) -> Pipe a b m ()
fromListT k = for cat (every . k)
{-# INLINABLE fromListT #-}

{-| Run an action with read-only state

> readOnly . return = return
>
> readOnly . (f >=> g) = (readOnly . f) >=> (readOnly . g)
-}
readOnly :: Monad m => ReaderT s m r -> StateT s m r
readOnly (R.ReaderT k) = S.StateT $ \s -> do
    r <- k s
    return (r, s)
{-# INLINABLE readOnly #-}

-- | A @(Managed a)@ is a resource @(a)@ bracketed by acquisition and release
newtype Managed a = Manage
    { -- | Consume a managed resource
      with :: forall x . (a -> IO x) -> IO x
    }

-- | Build a 'Managed' resource
manage :: (forall x . (a -> IO x) -> IO x) -> Managed a
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
    "Control.Monad.Trans.Reader" re-exports 'ReaderT' and 'Reader' (the types
    only), and 'ask'

    "Control.Monad.Trans.State.Strict" re-exports 'StateT' and 'State' (the
    types only), 'get', 'put', and 'modify'

    "Data.Functor.Constant" re-exports 'Constant' (the type only)

    "Data.Functor.Identity" re-exports 'Identity' (the type only)

    "Data.Monoid" re-exports 'Monoid', ('<>'), 'mconcat', and 'First' (the type
    only)

    "Lens.Family" re-exports 'LensLike'

    "Lens.Family.State.Strict" re-exports 'Zooming'

    All other modules re-export everything
-}
