module Dinote.Expression.Evaluate
  ( evaluate
  ) where

import Data.Foldable (sum)
import Dinote.Expression (Expression, ExpressionF(..))
import Dinote.Prelude
import Dinote.Vertex (VertexID(..))
import Global (readFloat)
import Matryoshka (cata)

evaluate :: (VertexID -> Maybe (Expression + String)) -> Expression -> String
evaluate vertices = cata evaluate'
  where
  evaluate' (Var name) =
    vertices (VertexID name)
    # fromMaybe (Right "")
    # either (evaluate vertices) id
  evaluate' (Sum values) =
    show <<< sum $ map readFloat values
