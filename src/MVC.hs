{-| Use the `Model` - `View` - `Controller` pattern to separate impure inputs
    and outputs from pure application logic so that you can:

    * Equationally reason about your model

    * Exercise your model with property-based testing (like @QuickCheck@)

    * Reproducibly replay your model

    The @mvc@ library uses the type system to statically enforce the separation
    of impure `View`s and `Controller`s from the pure `Model`.

    Here's a small example program written using the @mvc@ library:

> import MVC
> import qualified Pipes.Prelude as Pipes
>
> external :: Managed (View String, Controller String)
> external = do
>     c1 <- tick 1
>     c2 <- stdinLn
>     return (stdoutLn, fmap show c1 <> c2)
>
> model :: Model () String String
> model = pipe (Pipes.takeWhile (/= "quit"))
>     
> main :: IO ()
> main = runMVC () model external

    This program has three components:

    * A `Controller` that interleaves lines from standard input with periodic
      ticks

    * A `View` that writes lines to standard output

    * A pure `Model`, which forwards lines until the user inputs @"quit"@

    'runMVC' connects them into a complete program, which outputs a @()@ every
    second and also echoes standard input to standard output until the user
    enters @"quit"@:

>>> main
()
Test<Enter>
Test
()
()
42<Enter>
42
()
quit<enter>
>>>

    The following sections give extended guidance for how to structure @mvc@
    programs.  Additionally, there is an elaborate example using the @sdl@
    library in the \"Example\" section.
-}

{-# LANGUAGE RankNTypes #-}

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
    , pipe
    , loop

    -- * MVC
    -- $mvc
    , runMVC

    -- * Managed resources
    -- $managed
    , Managed
    , managed

    -- * Utilities
    , stdinLn
    , fromFile
    , tick
    , stdoutLn
    , toFile

    -- *ListT
    -- $listT

    -- * Example
    -- $example

    -- * Re-exports
    -- $reexports
    , module Data.Functor.Constant
    , module Data.Functor.Contravariant
    , module Data.Monoid
    , module Pipes
    , module Pipes.Concurrent
    ) where

import Control.Applicative (Applicative(pure, (<*>)), liftA2, (<*))
import Control.Category (Category(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad (join)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.State.Strict (State, execStateT)
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>), First)
import qualified Data.Monoid as M
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as Pipes
import qualified System.IO as IO

import Prelude hiding ((.), id)

{- $controller
    `Controller`s represent concurrent inputs to your system.  Use the `Functor`
    and `Monoid` instances for `Controller` and `Managed` to unify multiple
    `Managed` `Controller`s together into a single `Managed` `Controller`:

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

    Combining `Controller`s interleaves their values.
-}

{-| A concurrent source

> fmap f (c1 <> c2) = fmap f c1 <> fmap f c2
>
> fmap f mempty = mempty
-}
newtype Controller a = Controller (Input a)
-- This is just a newtype wrapper around `Input` because:
--
-- * I want the `Controller` name to "stick" in inferred types
--
-- * I want to restrict the API to ensure that `runMVC` is the only way to
--   consume `Controller`s.  This enforces strict separation of `Controller`
--   logic from `Model` or `View` logic

-- Deriving `Functor`
instance Functor Controller where
    fmap f (Controller i) = Controller (fmap f i)

-- Deriving `Monoid`
instance Monoid (Controller a) where
    mappend (Controller i1) (Controller i2) = Controller (mappend i1 i2)

    mempty = Controller mempty

{-| Create a `Controller` from a `Producer`, using the given `Buffer`

    If you're not sure what `Buffer` to use, try `Single`
-}
producer :: Buffer a -> Producer a IO () -> Managed (Controller a)
producer buffer prod = managed $ \k -> do
    (o, i, seal) <- spawn' buffer
    let io = do
            runEffect $ prod >-> toOutput o
            atomically seal
    withAsync io $ \_ -> k (input i) <* atomically seal
{-# INLINABLE producer #-}

-- | Create a `Controller` from an `Input`
input :: Input a -> Controller a
input = Controller
{-# INLINABLE input #-}

{- $view
    `View`s represent outputs of your system.  Use `handles` and the `Monoid`
    instance of `View` to unify multiple `View`s together into a single `View`:

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

    Combining `View`s sequences their outputs.

    If a @lens@ dependency is too heavy-weight, then you can manually generate
    `Traversal`s, which `handles` will also accept.  Here is an example of how
    you can generate `Traversal`s by hand with no dependencies:

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

    @(handles prism view)@ only runs the @view@ if the @prism@ matches the
    @view@'s input.

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
    -> View b
    -- ^
    -> View a
handles k (View send_) = View (\a -> case match a of
    Nothing -> return ()
    Just b  -> send_ b )
  where
    match = M.getFirst . getConstant . k (Constant . M.First . Just)
{-# INLINABLE handles #-}

-- | Convert a sink to a `View`
sink :: (a -> IO ()) -> View a
sink = View
{-# INLINABLE sink #-}

{- $model
    `Model`s are stateful streams and they sit in between `Controller`s and
    `View`s.

    Use `State` to internally communicate within the `Model`.  If you don't
    think you need it, you can enable hard mode by wrapping your model with
    @(hoist generalize)@.

    Read the \"ListT\" section which describes why you should prefer `ListT`
    over `Pipe` when possible.
-}

{-| A @(Model s a b)@ converts a stream of @(a)@s into a stream of @(b)@s while
    interacting with a state @(s)@
-}
newtype Model s a b = FromPipe (Pipe a b (State s) ())

instance Category (Model s) where
    (FromPipe m1) . (FromPipe m2) = FromPipe (m1 <-< m2)

    id = FromPipe cat

{-| Convert a `Pipe` to a `Model`

> pipe (p1 <-< p2) = pipe p1 . pipe p2
>
> pipe cat = id
-}
pipe :: Pipe a b (State s) () -> Model s a b
pipe = FromPipe
{-# INLINABLE pipe #-}

{-| Convert a `ListT` transformation to a `Pipe`

> loop (k1 >=> k2) = loop k1 >-> loop k2
>
> loop return = cat
-}
loop :: Monad m => (a -> ListT m b) -> Pipe a b m r
loop k = for cat (every . k)
{-# INLINABLE loop #-}


{- $mvc
    Connect a `Model`, `View`, and `Controller` and an initial state
    together using `runMVC` to complete your application.

    `runMVC` is the only way to consume `View`s and `Controller`s.  The types
    forbid you from mixing `View` and `Controller` logic with your `Model`
    logic.

    Note that `runMVC` only accepts one `View` and one `Controller`.  This
    enforces a single entry point and exit point for your `Model` so that you
    can cleanly separate your `Model` logic from your `View` logic and
    `Controller` logic.  The way you add more `View`s and `Controller`s to your
    program is by unifying them into a single `View` or `Controller` by using
    their `Monoid` instances.  See the \"Controllers\" and \"Views\" sections
    for more details on how to do this.
-}

{-| Connect a `Model`, `View`, and `Controller` and initial state into a
    complete application.
-}
runMVC
    :: s
    -- ^ Initial state
    -> Model s a b
    -- ^ Program logic
    -> Managed (View b, Controller a)
    -- ^ Effectful output and input
    -> IO s
    -- ^ Returns final state
runMVC initialState (FromPipe pipe_) viewController =
    _bind viewController $ \(View send_, Controller input_) ->
    flip execStateT initialState $ runEffect $
            fromInput input_
        >-> hoist (hoist generalize) pipe_
        >-> for cat (liftIO . send_)
{-# INLINABLE runMVC #-}

{- $managed
    Use `managed` to create primitive `Managed` resources and use the `Functor`,
    `Applicative`, `Monad`, and `Monoid` instances for `Managed` to bundle
    multiple `Managed` resources into a single `Managed` resource.

    See the source code for the \"Utilities\" section below for several examples
    of how to create `Managed` resources.

    Note that `runMVC` is the only way to consume `Managed` resources.
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

-- | Created a `Managed` resource
managed :: (forall x . (r -> IO x) -> IO x) -> Managed r
managed = Managed
{-# INLINABLE managed #-}

-- | Read lines from standard input
stdinLn :: Managed (Controller String)
stdinLn = producer Single Pipes.stdinLn
{-# INLINABLE stdinLn #-}

-- | Read lines from a file
fromFile :: FilePath -> Managed (Controller String)
fromFile filePath =
    join $ managed $ \k ->
        IO.withFile filePath IO.ReadMode $ \handle ->
            (k (producer Single (Pipes.fromHandle handle)))
{-# INLINABLE fromFile #-}

-- | Emit a values spaced by a delay in seconds
tick :: Double -> Managed (Controller ())
tick n = producer Single $ lift (threadDelay (truncate (n * 1000000))) >~ cat
{-# INLINABLE tick #-}

-- | Write lines to standard output
stdoutLn :: View String
stdoutLn = sink putStrLn
{-# INLINABLE stdoutLn #-}

-- | Write lines to a file
toFile :: FilePath -> Managed (View String)
toFile filePath =
    managed $ \k ->
        IO.withFile filePath IO.WriteMode $ \handle ->
            k (sink (IO.hPutStrLn handle))
{-# INLINABLE toFile #-}

{- $listT
    `ListT` computations can be combined in more ways than `Pipe`s, so try to
    program in `ListT` as much as possible and defer converting it to a `Pipe`
    as late as possible.

    You can combine `ListT` computations even if their inputs and outputs are
    completely different:

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
> model :: Model S TotalInput TotalOutput
> model = pipe (loop modelInToOut)

    Sometimes you have multiple computations that handle different inputs but
    the same output, in which case you don't need to unify their outputs:

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
> model :: Model S TotalInput TotalOutput
> model = pipe (loop modelInToOut)

    Other times you have multiple computations that handle the same input but
    produce different outputs.  You can unify their outputs using the `Monoid`
    and `Functor` instances for `ListT`:

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
> model :: Model S TotalInput TotalOutput
> model = pipe (loop modelInToOut)

    You can also chain `ListT` computations, feeding the output of the first
    computation as the input to the next computation:

> -- End-to-end
>
> modelInToMiddle  :: TotalInput -> ListT (State S) MiddleStep
> modelMiddleToOut :: MiddleStep -> ListT (State S) TotalOutput
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut = modelInToMiddle >=> modelMiddleToOut
>
> model :: Model S TotalInput TotalOutput
> model = pipe (loop modelInToOut)

    ... or you can just use @do@ notation if you prefer.

    However, the `Pipe` type is more general than `ListT` and can represent
    things like termination.  Therefore you should consider mixing `Pipe`s with
    `ListT` when you need to take advantage of these extra features:

> -- Mix ListT with Pipes
>
> model :: Model S TotalInput TotalOutput
> model = pipe (Pipes.takeWhile (/= C) >-> loop modelInToOut)

    So promote your `ListT` logic to a `Pipe` when you need to take advantage of
    these `Pipe`-specific features.
-}

{- $example
    The following example distils a @sdl@-based program into pure and impure
    components.  This program will draw a white rectangle between every two
    mouse clicks.

    The first half of the program contains all the concurrent and impure logic.
    The `View` and `Controller` must be `Managed` together since they both share
    the same initialization logic:

> import Control.Monad (join)
> import Graphics.UI.SDL as SDL
> import Lens.Family.Stock (_Left, _Right)
> import MVC
> import qualified Pipes.Prelude as Pipes
> 
> data Done = Done deriving (Eq, Show)
> 
> sdl :: Managed (View (Either Rect Done), Controller Event)
> sdl = join $ managed $ \k -> withInit [InitEverything] $ do
>     surface <- setVideoMode 640 480 32 [SWSurface]
>     white   <- mapRGB (surfaceGetPixelFormat surface) 255 255 255
> 
>     let done :: View Done
>         done = sink (\Done -> SDL.quit)
> 
>         drawRect :: View Rect
>         drawRect = sink $ \rect -> do
>             _ <- fillRect surface (Just rect) white
>             SDL.flip surface
> 
>         totalOut :: View (Either Rect Done)
>         totalOut = handles _Left drawRect <> handles _Right done
> 
>     k $ do
>         totalIn <- producer Single (lift waitEvent >~ cat)
>         return (totalOut, totalIn)

    Note the `join` surrounding the `managed` block.  This is because the type
    before `join` is:

> Managed (Managed (View (Either Rect Done), Controller Event))

    More generally, note that `Managed` is a `Monad`, so you can use @do@
    notation to combine multiple `Managed` resources into a single `Managed`
    resource.

    The second half of the program contains the pure logic.

> model :: Monad m => Model () Event (Either Rect Done)
> model = pipe $ do
>     Pipes.takeWhile (/= Quit) >-> (click >~ rectangle >~ Pipes.map Left)
>     yield (Right Done)
> 
> rectangle :: Monad m => Consumer' (Int, Int) m Rect
> rectangle = do
>     (x1, y1) <- await
>     (x2, y2) <- await
>     let x = min x1 x2
>         y = min y1 y2
>         w = abs (x1 - x2)
>         h = abs (y1 - y2)
>     return (Rect x y w h)
> 
> click :: Monad m => Consumer' Event m (Int, Int)
> click = do
>     e <- await
>     case e of
>         MouseButtonDown x y ButtonLeft ->
>             return (fromIntegral x, fromIntegral y)
>         _ -> click
> 
> main :: IO ()
> main = runMVC () model sdl

    Run the program to verify that clicks create rectangles.

    The more logic you move into the pure core the more you can exercise your
    program purely, either manually:

>>> let leftClick (x, y) = MouseButtonDown x y ButtonLeft
>>> Pipes.toList (each [leftClick (10, 10), leftClick (15, 16), Quit] >-> model)
[Left (Rect {rectX = 10, rectY = 10, rectW = 5, rectH = 6}),Right Done]

    ... or automatically using property-based testing (such as @QuickCheck@):

>>> import Test.QuickCheck
>>> quickCheck $ \xs -> length (Pipes.toList (each (map leftClick xs) >-> model)) == length xs `div` 2
+++ OK, passed 100 tests.

    Equally important, you can formally prove properties about your model using
    equational reasoning because the model is `IO`-free and concurrency-free.
-}

{- $reexports
    "Data.Functor.Constant" re-exports `Constant` (the type only)

    "Data.Functor.Contravariant" re-exports `Contravariant`

    "Data.Monoid" re-exports `Monoid`, (`<>`), `mconcat`, and `First` (the type
    only)

    "Pipes" re-exports everything

    "Pipes.Concurrent" re-exports everything
-}
