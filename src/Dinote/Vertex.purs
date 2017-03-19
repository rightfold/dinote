module Dinote.Vertex
  ( VertexID(..)
  , Vertex
  , vertexBody
  ) where

import Data.Lens (Lens', lens)
import Dinote.Expression (Expression)
import Dinote.Prelude

newtype VertexID = VertexID String
derive newtype instance eqVertexID :: Eq VertexID
derive newtype instance ordVertexID :: Ord VertexID

newtype Vertex = Vertex (Expression + String)

vertexBody :: Lens' Vertex (Expression + String)
vertexBody = lens get set
  where get (Vertex body) = body
        set (Vertex _) body = Vertex body
