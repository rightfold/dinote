module Dinote.Prelude
  ( module Control.Monad.Free
  , module Data.Const
  , module Data.List
  , module Data.Map
  , module Data.Maybe
  , module Data.Newtype
  , module Data.Tuple.Nested
  , module Debug.Trace
  , module Prelude
  , type (*)
  , type (+)
  , type (<+>)
  ) where

import Control.Monad.Free (Free, foldFree, liftF)
import Data.Const (Const)
import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceAnyA)
import Prelude

infixr 7 type Tuple as *
infixr 6 type Either as +
infixr 6 type Coproduct as <+>
