module Dinote.Vertex
  ( VertexID(..)
  , Vertex
  , vertexID
  , vertexBody
  ) where

import Data.Lens (Lens', lens)
import Dinote.Expression (Expression)
import Dinote.Prelude

newtype VertexID = VertexID String
derive newtype instance eqVertexID :: Eq VertexID
derive newtype instance ordVertexID :: Ord VertexID

data Vertex = Vertex VertexID (Expression + String)

vertexID :: Lens' Vertex VertexID
vertexID = lens get set
  where get (Vertex id _) = id
        set (Vertex _ body) id = Vertex id body

vertexBody :: Lens' Vertex (Expression + String)
vertexBody = lens get set
  where get (Vertex _ body) = body
        set (Vertex id _) body = Vertex id body
