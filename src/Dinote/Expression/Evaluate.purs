module Dinote.Expression.Evaluate
  ( evaluate
  ) where

import Data.Foldable (sum)
import Data.Map as Map
import Dinote.Expression (Expression, ExpressionF(..))
import Dinote.Prelude
import Dinote.Vertex (VertexID(..))
import Global (readFloat)
import Matryoshka (cata)

evaluate :: Map VertexID (Expression + String) -> Expression -> String
evaluate vertices = cata evaluate'
  where
  evaluate' (Var name) =
    Map.lookup (VertexID name) vertices
    # fromMaybe (Right "")
    # either (evaluate vertices) id
  evaluate' (Sum values) =
    show <<< sum $ map readFloat values
