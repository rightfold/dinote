module Dinote.Expression.Evaluate
  ( evaluate
  ) where

import Data.Foldable (sum)
import Data.Int as Int
import Data.Ratio (Ratio(..))
import Data.Rational (Rational(..), (%))
import Data.String as String
import Dinote.Expression (Expression, ExpressionF(..))
import Dinote.Prelude
import Dinote.Vertex (VertexID(..))
import Matryoshka (cata)

evaluate :: (VertexID -> Maybe (Expression + String)) -> Expression -> String
evaluate vertices = cata evaluate'
  where
  evaluate' (Var name) =
    vertices (VertexID name)
    # fromMaybe (Right "")
    # either (evaluate vertices) id
  evaluate' (Sum values) =
    showRational <<< sum $ map (fromMaybe zero <<< readRational) values

showRational :: Rational -> String
showRational (Rational (Ratio a 1)) = show a
showRational (Rational (Ratio a b)) = show a <> "/" <> show b

readRational :: String -> Maybe Rational
readRational = (<|>) <$> withPoint <*> withSlash
  where
  withPoint =
    String.split (String.Pattern ".")
    >>> traverse Int.fromString
    >=> case _ of
          [a]    -> Just $ a % 1
          [a, b] -> Just $ a % 1 + b % Int.pow 10 (String.length (show b))
          _      -> Nothing
  withSlash =
    String.split (String.Pattern "/")
    >>> traverse Int.fromString
    >=> case _ of
          [a]    -> Just $ a % 1
          [a, b] -> Just $ a % b
          _      -> Nothing
