{-| Use the 'Model' - 'View' - 'Controller' pattern to separate impure inputs
    and outputs from pure application logic.
-}

{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module MVC (
    -- * Controllers
    -- $controller
      Controller
    , producer
    , managedProducer
    , managedInput
    , tick
    , stdinLn

    -- * Views
    -- $view
    , View
    , handles
    , handling
    , sink
    , managedSink
    , stdoutLn

    -- * Models
    -- $model
    , Model
    , runMVC
    , listT
    -- $listT

    -- * Re-exports
    -- $reexports
    , module Data.Functor.Constant
    , module Data.Functor.Contravariant
    , module Data.Monoid
    , module Pipes
    , module Pipes.Concurrent
    ) where

import Control.Applicative (pure, liftA2, (<*))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.State.Strict (State, execStateT)
import Control.Monad.Trans.Cont (ContT(ContT, runContT))
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>), First)
import qualified Data.Monoid as M
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as Pipes

{- $controller
    'Controller's represent concurrent inputs to your system.  Use the 'Functor'
    and 'Monoid' instances of 'Controller' to bundle multiple 'Controller's
    together:

> controllerA :: Controller A
> controllerB :: Controller B
> controllerC :: Controller C
>
> data TotalInput = InA A | InB B | InC C
>
> controllerTotal :: Controller TotalInput
> controllerTotal =
>         fmap InA controllerA
>     <>  fmap InB controllerB
>     <>  fmap InC controllerC
-}

{-| A managed, concurrent input

> fmap f (c1 <> c2) = fmap f c1 <> fmap f c2
>
> fmap f mempty = mempty
-}
newtype Controller a = Controller (forall x . ContT x IO (Input a))

instance Functor Controller where
    fmap f (Controller c) = Controller (fmap (fmap f) c)

instance Monoid (Controller a) where
    mempty = Controller (pure mempty)
    mappend (Controller c1) (Controller c2) = Controller (liftA2 mappend c1 c2)

-- | Create a 'Controller' from a 'Producer', using the given 'Buffer'
producer :: Buffer a -> Producer a IO () -> Controller a
producer buffer prod =
    managedInput $ \k -> do
        (output, input, seal) <- spawn' buffer
        let io = do
                runEffect $ prod >-> toOutput output
                atomically seal
        withAsync io $ \_ -> k input <* atomically seal
{-# INLINABLE producer #-}

-- | Emit a values spaced by a delay in seconds
tick :: Double -> Controller ()
tick n = producer Unbounded $ lift (threadDelay (truncate (n * 1000000))) >~ cat
{-# INLINABLE tick #-}

-- | Read lines from standard input
stdinLn :: Controller String
stdinLn = producer Unbounded Pipes.stdinLn
{-# INLINABLE stdinLn #-}

-- | Create a 'Controller' from a managed 'Producer', using the given 'Buffer'
managedProducer
    :: Buffer a
    -> (forall x . (Producer a IO () -> IO x) -> IO x)
    -> Controller a
managedProducer buffer withProducer =
    managedInput $ \k -> do
    withProducer $ \prod -> do
        (output, input, seal) <- spawn' buffer
        let io = do
                runEffect $ prod >-> toOutput output
                atomically seal
        withAsync io $ \_ -> k input <* atomically seal

-- | Create a 'Controller' from a managed 'Input'
managedInput :: (forall x . (Input a -> IO x) -> IO x) -> Controller a
managedInput k = Controller (ContT k)
{-# INLINABLE managedInput #-}

{-
record :: Binary a => FilePath -> Controller a -> Controller a
record filePath =
    Controller $ ContT $ \send ->
    withFile filePath IO.WriteMode $ \handle -> k $ \a -> do
-}

{- $view
    'View's represent outputs of your system.  Use 'handling' and the 'Monoid'
    instance of 'View' to combine multiple 'View's together into a single 'View'
    using prisms:

> viewD :: View D
> viewE :: View E
> viewF :: View F
>
> data TotalOutput = OutD D | OutE E | OutF F
>
> makePrisms ''TotalOutput  -- Generates _OutD, _OutE, and _OutF prisms
>
> viewTotal :: View TotalOutput
> viewTotal =
>         handling _OutD viewD
>     <>  handling _OutE viewE
>     <>  handling _OutF viewF

    If a @lens@ dependency is too heavy-weight, then use the 'handles' function
    instead.
-}

newtype Action = Action { runAction :: IO () }

instance Monoid Action where
    mempty = Action (return ())
    mappend (Action io1) (Action io2) = Action (io1 >> io2)

newtype Sink a = Sink (a -> Action)

instance Monoid (Sink a) where
    mempty = Sink (pure mempty)
    mappend (Sink write1) (Sink write2) = Sink (liftA2 mappend write1 write2)

instance Contravariant Sink where
    contramap f (Sink k) = Sink (k . f)

{-| A managed sink

> contramap f (v1 <> v2) = contramap f v1 <> contramap f v2
>
> contramap f mempty  mempty
-}
newtype View a = View (forall x . ContT x IO (Sink a))

instance Monoid (View a) where
    mempty = View (pure mempty)
    mappend (View c1) (View c2) = View (liftA2 mappend c1 c2)

instance Contravariant View where
    contramap f (View c) = View (fmap (contramap f) c)

{-| Pre-map a partial getter to define a partial handler

> handles (f <=< g) = handles g . handles f
>
> handles return = id

> handles f (v1 <> v2) = handles f v1 <> handles f v2
>
> handles f mempty = mempty
-}
handles :: (a -> Maybe b) -> View b -> View a
handles f (View c) = View (fmap onJust c)
  where
    onJust (Sink send_) = Sink (\a -> case f a of
        Nothing -> mempty
        Just b  -> send_ b )
{-# INLINABLE handles #-}

{-| This is a variation on 'handles' designed to work with prisms auto-generated
    by the @lens@ library.  Think of the type as one of the following types:

> handling :: Prism'     a b -> View b -> View a
> handling :: Traversal' a b -> View b -> View a

    @(handling prism action)@ only runs the @action@ if the @prism@ matches the
    @action@'s input, using the prism to transform the input.

> handling (p1 . p2) = handling p2 . handling p1
>
> handling id = id

> handling f (v1 <> v2) = handling f v1 <> handling f v2
>
> handling f mempty = mempty
-}
handling
    :: ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> (View b -> View a)
    -- ^
handling k = handles (M.getFirst . getConstant . k (Constant . M.First . Just))
{-# INLINABLE handling #-}

-- | Convert a sink to a 'View'
sink :: (a -> IO ()) -> View a
sink handler = managedSink (\k -> k handler)
{-# INLINABLE sink #-}

-- | Convert a managed sink to a 'View'
managedSink :: (forall x . ((a -> IO ()) -> IO x) -> IO x) -> View a
managedSink k = View (fmap (\f -> Sink (\a -> Action (f a))) (ContT k))
{-# INLINABLE managedSink #-}

-- | Write lines to standard output
stdoutLn :: View String
stdoutLn = sink putStrLn
{-# INLINABLE stdoutLn #-}

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
-}

{-| A @(Model s a b)@ converts a stream of @(a)@s into a stream of @(b)@s while
    interacting with a state @(s)@
-}
type Model s a b = Pipe a b (State s) ()

{-| Connect a 'Model', 'View', and 'Controller' and initial state into a
    complete application.
-}
runMVC
    :: Controller a
    -- ^ Effectful input
    -> Model s a b
    -- ^ Program logic
    -> View b
    -- ^ Effectful output
    -> s
    -- ^ Initial state
    -> IO s
runMVC (Controller c) model (View v) initialState =
    runContT c $ \input ->
    runContT v $ \(Sink send_)  ->
    flip execStateT initialState $ runEffect $
            fromInput input
        >-> hoist (hoist generalize) model
        >-> for cat (\a -> liftIO (runAction (send_ a)))
{-# INLINABLE runMVC #-}

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

{- $reexports
    "Data.Functor.Constant" re-exports 'Constant' (the type only)

    "Data.Functor.Contravariant" re-exports 'Contravariant'

    "Data.Monoid" re-exports 'Monoid', ('<>'), 'mconcat', and 'First' (the type
    only)

    "Pipes" re-exports everything

    "Pipes.Concurrent" re-exports everything
-}
