{-# LANGUAGE RankNTypes #-}

{-| I prefer not to export this module.  Use `MVC.Prism`, which provides a
    public and forwards-compatible API for more complex operations.
-}

module MVC.Internal (
    -- * Types
      Model(..)
    , View(..)
    , Controller(..)
    , Managed(..)
    ) where

import Control.Applicative (Applicative(pure, (<*>)), liftA2)
import Control.Category (Category((.), id))
import Control.Monad.Trans.State.Strict (State)
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Monoid (Monoid(mappend, mempty))
import Pipes (Pipe, (<-<), cat)
import Pipes.Concurrent (Input)

import Prelude hiding ((.), id)

{-| A @(Model s a b)@ converts a stream of @(a)@s into a stream of @(b)@s while
    interacting with a state @(s)@
-}
newtype Model s a b = AsPipe (Pipe a b (State s) ())

instance Category (Model s) where
    (AsPipe m1) . (AsPipe m2) = AsPipe (m1 <-< m2)

    id = AsPipe cat

{-| An effectful sink

> contramap f (v1 <> v2) = contramap f v1 <> contramap f v2
>
> contramap f mempty = mempty
-}
newtype View a = AsSink (a -> IO ())

instance Monoid (View a) where
    mempty = AsSink (\_ -> return ())
    mappend (AsSink write1) (AsSink write2) = AsSink (\a -> write1 a >> write2 a)

instance Contravariant View where
    contramap f (AsSink k) = AsSink (k . f)

{-| A concurrent source

> fmap f (c1 <> c2) = fmap f c1 <> fmap f c2
>
> fmap f mempty = mempty
-}
newtype Controller a = AsInput (Input a)
-- This is just a newtype wrapper around `Input` because:
--
-- * I want the `Controller` name to "stick" in inferred types
--
-- * I want to restrict the API to ensure that `runMVC` is the only way to
--   consume `Controller`s.  This enforces strict separation of `Controller`
--   logic from `Model` or `View` logic

-- Deriving `Functor`
instance Functor Controller where
    fmap f (AsInput i) = AsInput (fmap f i)

-- Deriving `Monoid`
instance Monoid (Controller a) where
    mappend (AsInput i1) (AsInput i2) = AsInput (mappend i1 i2)

    mempty = AsInput mempty

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
