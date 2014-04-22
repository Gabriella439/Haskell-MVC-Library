{-# LANGUAGE RankNTypes #-}

{-| This module provides a public and forwards-compatible API for accessing
    more complex functionality.

    The most common ways to use prisms are:

    * Use 'Control.Lens.review' to use a prism as a constructor

    * Use 'Control.Lens.preview' to use a prism as partial getter that returns
      'Nothing' if the target is not present

    * Use 'Control.Lens.over' to map functions over the target of the prism

    This prism-based API lets me extend the underlying types if necessary
    without breaking downstream code.
-}

module MVC.Prism (
    -- * Prisms
      _AsPipe
    , _AsSink
    , _AsInput
    ) where

import Control.Applicative (Applicative(pure))
import Control.Monad.Trans.State.Strict (State)
import Data.Profunctor (Choice(right'), dimap)
import Pipes (Pipe)
import Pipes.Concurrent (Input)

import MVC.Internal(Model(..), View(..), Controller(..))

type Prism s t a b =
    forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

{-| 'Control.Lens.Prism' from a 'Model' to a 'Pipe'

> _AsPipe :: Prism' (Model s a b) (Pipe a b (State s) ())
-}
_AsPipe
    :: Prism
        (Model s  a  b )
        (Model s' a' b')
        (Pipe a  b  (State s ) ())
        (Pipe a' b' (State s') ())
_AsPipe = dimap (\(AsPipe x) -> Right x) (either pure (fmap AsPipe)) . right'
{-# INLINABLE _AsPipe #-}

{-| 'Control.Lens.Prism' from a 'View' to a sink

> _AsSink :: Prism' (View a) (a -> IO ())
-}
_AsSink :: Prism (View a) (View a') (a -> IO ()) (a' -> IO ())
_AsSink = dimap (\(AsSink x) -> Right x) (either pure (fmap AsSink)) . right'
{-# INLINABLE _AsSink #-}

{-| 'Control.Lens.Prism' from a 'Controller' to an 'Input'

> _AsInput :: Prism' (Controller a) (Input a)
-}
_AsInput :: Prism (Controller a) (Controller a') (Input a) (Input a')
_AsInput = dimap (\(AsInput x) -> Right x) (either pure (fmap AsInput)) . right'
{-# INLINABLE _AsInput #-}
