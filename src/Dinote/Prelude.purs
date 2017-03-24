module Dinote.Prelude
  ( module Control.Alt
  , module Control.Monad.Free
  , module Data.Const
  , module Data.Either
  , module Data.Foldable
  , module Data.List
  , module Data.Map
  , module Data.Maybe
  , module Data.Newtype
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Debug.Trace
  , module Prelude
  , type (*)
  , type (+)
  , type (<+>)
  ) where

import Control.Alt ((<|>))
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Functor.Coproduct (Coproduct)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceAnyA)
import Prelude

infixr 7 type Tuple as *
infixr 6 type Either as +
infixr 6 type Coproduct as <+>
