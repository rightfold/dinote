module Dinote.Expression.Parse
  ( parse
  ) where

import Data.Functor.Mu (roll)
import Data.Sexp (Sexp(..))
import Data.Sexp as Sexp
import Data.String as String
import Dinote.Expression (Expression, ExpressionF(..))
import Dinote.Prelude

parse :: String -> Maybe Expression
parse = Sexp.fromString >=> parse'

parse' :: Sexp -> Maybe Expression
parse' (Atom x) = case String.uncons x of
  Just {head: '$', tail} -> Just <<< roll $ Var tail
  _ -> Nothing
parse' (List (Atom "+" : xs)) = roll <<< Sum <$> traverse parse' xs
parse' _ = Nothing
