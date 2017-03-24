module Dinote.Expression
  ( Expression
  , ExpressionF(..)
  , pretty
  ) where

import Data.Functor.Mu (Mu)
import Dinote.Prelude
import Matryoshka (cata)

type Expression = Mu ExpressionF

data ExpressionF a
  = Var String
  | Sum (List a)

derive instance functorExpressionF :: Functor ExpressionF

pretty :: Expression -> String
pretty = cata pretty'
  where
  pretty' (Var name) = "$" <> name
  pretty' (Sum values) = "SUM(" <> intercalate ", " values <> ")"
