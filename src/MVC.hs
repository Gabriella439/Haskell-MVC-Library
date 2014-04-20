{-| Use the 'Model' - 'View' - 'Controller' pattern to separate impure inputs
    and outputs from pure application logic.
-}

{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

module MVC (
    -- * Controllers
    -- $controller
      Controller
    , producer
    , input

    -- * Views
    -- $view
    , View
    , handles
    , sink

    -- * Models
    -- $model
    , Model
    , runMVC

    -- * Utilities
    -- $utilities
    , view
    , controller
    , listT
    -- $listT

    -- * Managed Resources
    , Managed
    , managed

    -- * Examples
    , stdinLn
    , tick
    , stdoutLn

    -- * Re-exports
    -- $reexports
    , module Data.Functor.Constant
    , module Data.Functor.Contravariant
    , module Data.Monoid
    , module Pipes
    , module Pipes.Concurrent
    ) where

import Control.Applicative (Applicative(pure, (<*>)), liftA2, (<*))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.State.Strict (State, execStateT)
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>), First)
import qualified Data.Monoid as M
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as Pipes

{- $controller
    'Controller's represent concurrent inputs to your system.  Use the 'Functor'
    and 'Monoid' instances of 'Controller' to unify multiple 'Controller's
    together into a single 'Controller':

> controllerA :: Managed (Controller A)
> controllerB :: Managed (Controller B)
> controllerC :: Managed (Controller C)
>
> data TotalInput = InA A | InB B | InC C
>
> controllerTotal :: Managed (Controller TotalInput)
> controllerTotal =
>         fmap (fmap InA) controllerA
>     <>  fmap (fmap InB) controllerB
>     <>  fmap (fmap InC) controllerC
-}

{-| A concurrent source

> fmap f (c1 <> c2) = fmap f c1 <> fmap f c2
>
> fmap f mempty = mempty
-}
newtype Controller a = Controller (Input a) deriving (Functor, Monoid)
-- This is just a newtype wrapper around `Input` because:
--
-- * I want the `Controller` name to "stick" in inferred types
--
-- * I want to restrict the API to ensure that `runMVC` is the only way to
--   consume `Controller`s.  This enforces strict separation of `Controller`
--   logic from `Model` or `View` logic

{-| Create a 'Controller' from a 'Producer', using the given 'Buffer'

    If you're not sure what 'Buffer' to use, try 'Single'
-}
producer :: Buffer a -> Producer a IO () -> Managed (Controller a)
producer buffer prod = managed $ \k -> do
    (o, i, seal) <- spawn' buffer
    let io = do
            runEffect $ prod >-> toOutput o
            atomically seal
    withAsync io $ \_ -> k (input i) <* atomically seal
{-# INLINABLE producer #-}

-- | Create a 'Controller' from an 'Input'
input :: Input a -> Controller a
input = Controller
{-# INLINABLE input #-}

{- $view
    'View's represent outputs of your system.  Use 'handles' and the 'Monoid'
    instance of 'View' to unify multiple 'View's together into a single 'View':

> viewD :: Managed (View D)
> viewE :: Managed (View E)
> viewF :: Managed (View F)
>
> data TotalOutput = OutD D | OutE E | OutF F
>
> makePrisms ''TotalOutput  -- Generates _OutD, _OutE, and _OutF prisms
>
> viewTotal :: Managed (View TotalOutput)
> viewTotal =
>         fmap (handles _OutD) viewD
>     <>  fmap (handles _OutE) viewE
>     <>  fmap (handles _OutF) viewF

    If a @lens@ dependency is too heavy-weight, then you can manually generate
    'Traversal's by hand, which 'handles' also accepts.  Here is an example of
    how you would do that:

> -- _OutD :: Traversal' TotalOutput D
> _OutD :: Applicative f => (D -> f D) -> (TotalOutput -> f TotalOutput)
> _OutD k (OutD d) = fmap OutD (k d)
> _OutD k  t       = pure t
>
> -- _OutE :: Traversal' TotalOutput E
> _OutE :: Applicative f => (E -> f E) -> (TotalOutput -> f TotalOutput)
> _OutE k (OutE d) = fmap OutE (k d)
> _OutE k  t       = pure t
>
> -- _OutF :: Traversal' TotalOutput F
> _OutF :: Applicative f => (F -> f F) -> (TotalOutput -> f TotalOutput)
> _OutF k (OutF d) = fmap OutF (k d)
> _OutF k  t       = pure t

    instead.  Or you can manually
-}

{-| An effectful sink

> contramap f (v1 <> v2) = contramap f v1 <> contramap f v2
>
> contramap f mempty = mempty
-}
newtype View a = View (a -> IO ())

instance Monoid (View a) where
    mempty = View (\_ -> return ())
    mappend (View write1) (View write2) = View (\a -> write1 a >> write2 a)

instance Contravariant View where
    contramap f (View k) = View (k . f)

{-| Think of the type as one of the following types:

> handles :: Prism'     a b -> View b -> View a
> handles :: Traversal' a b -> View b -> View a

    @(handles prism action)@ only runs the @action@ if the @prism@ matches the
    @action@'s input.

> handles (p1 . p2) = handles p2 . handles p1
>
> handles id = id

> handles f (v1 <> v2) = handles f v1 <> handles f v2
>
> handles f mempty = mempty
-}
handles
    :: ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> (View b -> View a)
    -- ^
handles k (View send_) = View (\a -> case match a of
    Nothing -> return ()
    Just b  -> send_ b )
  where
    match = M.getFirst . getConstant . k (Constant . M.First . Just)
{-# INLINABLE handles #-}

-- | Convert a sink to a 'View'
sink :: (a -> IO ()) -> View a
sink = View
{-# INLINABLE sink #-}

{- $model
    'Model's are stateful streams and they sit in between 'Controller's and
    'View's.  Connect a 'Model', 'View', and 'Controller' and an initial state
    together using 'runMVC' to complete your application.

    The 'Model' is designed to be pure, reproducible, and concurrency-free so
    that you can:

    * @QuickCheck@ your model,

    * equationally reason about its behavior, and:

    * replay the model deterministically.

    Use 'State' to internally communicate within the 'Model'.  If you don't
    think you need it, you can enable hard mode by wrapping your model with
    @(hoist generalize)@.

    'runMVC' is the only way to consume 'View's and 'Controller's.  The types
    forbid you from mixing 'View' and 'Controller' logic with your 'Model'
    logic.
-}

{-| A @(Model s a b)@ converts a stream of @(a)@s into a stream of @(b)@s while
    interacting with a state @(s)@
-}
type Model s a b = Pipe a b (State s) ()

{-| Connect a 'Model', 'View', and 'Controller' and initial state into a
    complete application.
-}
runMVC
    :: Model s a b
    -- ^ Program logic
    -> Managed (View b, Controller a)
    -- ^ Effectful output and input
    -> s
    -- ^ Initial state
    -> IO s
runMVC model viewController initialState =
    _bind viewController $ \(View send_, Controller input_) ->
    flip execStateT initialState $ runEffect $
            fromInput input_
        >-> hoist (hoist generalize) model
        >-> for cat (liftIO . send_)
{-# INLINABLE runMVC #-}

{- $utilities
    Use 'view' and 'controller' to mix 'View's and 'Controller's using
    'Monoid' syntax:

> viewControllerTotal :: Managed (View TotalOutput, Controller TotalInput)
> viewControllerTotal = fmap view viewTotal <> fmap controller controllerTotal
-}

{-| Generalize a 'View' to a 'View' and 'Controller'

> view (v1 <> v2) = view v1 <> view v2
>
> view mempty = mempty
-}
view :: View b -> (View b, Controller a)
view v = (v, mempty)
{-# INLINABLE view #-}

{-| Generalize a 'Controller' to a 'View' and 'Controller'

> controller (c1 <> c2) = controller c1 <> controller c2
>
> controller mempty = mempty
-}
controller :: Controller a -> (View b, Controller a)
controller c = (mempty, c)
{-# INLINABLE controller #-}

{-| Convert a 'ListT' transformation to a 'Pipe'

> listT (k1 >=> k2) = listT k1 >-> listT k2
>
> listT return = cat
-}
listT :: Monad m => (a -> ListT m b) -> Pipe a b m r
listT k = for cat (every . k)
{-# INLINABLE listT #-}

{- $listT
    'ListT' computations can be combined in more ways than 'Pipe's, so try to
    program in 'ListT' as much as possible and defer converting it to a 'Pipe'
    as late as possible.

    Here are some examples for how to combine 'ListT' computations into larger
    computations:

> -- Independent computations
>
> modelAToD :: A -> ListT (State S) D
> modelBToE :: B -> ListT (State S) E
> modelCToF :: C -> ListT (State s) F
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut totalInput = case totalInput of
>     InA a -> fmap OutD (modelAToD a)
>     InB b -> fmap OutE (modelAToD b)
>     InC c -> fmap OutF (modelAToD c)
>
> model :: Pipe TotalInput TotalOutput (State S) r
> model = listT modelInToOut

> -- Overlapping outputs
>
> modelAToOut :: A -> ListT (State S) Out
> modelBToOut :: B -> ListT (State S) Out
> modelCToOut :: C -> ListT (State S) Out
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut totalInput = case totalInput of
>     InA a -> modelAToOut a
>     InB b -> modelBToOut b
>     InC c -> modelBToOut b
>
> model :: Pipe TotalInput TotalOutput (State S) r
> model = listT modelInToOut

> -- Overlapping inputs
>
> modelInToA :: TotalInput -> ListT (State S) A
> modelInToB :: TotalInput -> ListT (State S) B
> modelInToC :: TotalInput -> ListT (State S) C
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut totalInput =
>        fmap OutA (modelInToA totalInput)
>     <> fmap OutB (modelInToB totalInput)
>     <> fmap OutC (modelInToC totalInput)
>
> model :: Pipe TotalInput TotalOutput (State S) r
> model = listT modelInToOut

> -- End-to-end
>
> modelInToMiddle  :: TotalInput -> ListT (State S) MiddleStep
> modelMiddleToOut :: MiddleStep -> ListT (State S) TotalOutput
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut = modelInToMiddle >=> modelMiddleToOut
>
> model :: Pipe TotalInput TotalOutput (State S) r
> model = listT modelInToOut

    However, 'Pipe' is more general than 'ListT' and can represent things like
    termination:

> -- Mix ListT with Pipes to terminate on a given input
>
> quitOnC :: Monad m => Pipe TotalInput TotalInput m ()
> quitOnC = do
>     totalInput <- await
>     case totalInput of
>         C -> return ()
>         _ -> do
>             yield totalInput
>             quitOnC
>
> model :: Pipe TotalInput TotalOutput (State S) ()
> model = quitOnC >-> listT modelInToOut

    So promote your 'ListT' logic to a 'Pipe' when you need to take advantage of
    these 'Pipe'-specific features.
-}

-- | A managed resource
newtype Managed r = Managed { _bind :: forall x . (r -> IO x) -> IO x }
-- `Managed` is the same thing as `Codensity IO` or `forall x . ContT x IO`
--
-- I implement a custom type instead of reusing those types because:
--
-- * I need a non-orphan `Monoid` instance
--
-- * The name and type are simpler

instance Functor Managed where
    fmap f mx = Managed (\_return ->
        _bind mx (\x ->
        _return (f x) ) )

instance Applicative Managed where
    pure r    = Managed (\_return ->
        _return r )
    mf <*> mx = Managed (\_return ->
        _bind mf (\f ->
        _bind mx (\x ->
        _return (f x) ) ) )

instance Monad Managed where
    return r = Managed (\_return ->
        _return r )
    ma >>= f = Managed (\_return ->
        _bind  ma   (\a ->
        _bind (f a) (\b ->
        _return b ) ) )

instance Monoid r => Monoid (Managed r) where
    mempty  = pure mempty
    mappend = liftA2 mappend

-- | Created a 'Managed' resource
managed :: (forall x . (r -> IO x) -> IO x) -> Managed r
managed = Managed

-- | Read lines from standard input
stdinLn :: Managed (Controller String)
stdinLn = producer Unbounded Pipes.stdinLn
{-# INLINABLE stdinLn #-}

-- | Emit a values spaced by a delay in seconds
tick :: Double -> Managed (Controller ())
tick n = producer Unbounded $ lift (threadDelay (truncate (n * 1000000))) >~ cat
{-# INLINABLE tick #-}

-- | Write lines to standard output
stdoutLn :: View String
stdoutLn = sink putStrLn
{-# INLINABLE stdoutLn #-}

{- $reexports
    "Data.Functor.Constant" re-exports 'Constant' (the type only)

    "Data.Functor.Contravariant" re-exports 'Contravariant'

    "Data.Monoid" re-exports 'Monoid', ('<>'), 'mconcat', and 'First' (the type
    only)

    "Pipes" re-exports everything

    "Pipes.Concurrent" re-exports everything
-}
