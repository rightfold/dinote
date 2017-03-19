module Dinote.Expression
  ( Expression
  , ExpressionF(..)
  ) where

import Data.Functor.Mu (Mu)
import Dinote.Prelude

type Expression = Mu ExpressionF

data ExpressionF a
  = Var String

derive instance functorExpressionF :: Functor ExpressionF
