module Dinote.Vertex
  ( VertexID(..)
  , Vertex(..)
  , vertexBody
  , vertexChildren
  ) where

import Data.Lens (Lens', lens)
import Dinote.Expression (Expression)
import Dinote.Prelude

newtype VertexID = VertexID String
derive newtype instance eqVertexID :: Eq VertexID
derive newtype instance ordVertexID :: Ord VertexID

data Vertex = Vertex (Expression + String) (List VertexID)

vertexBody :: Lens' Vertex (Expression + String)
vertexBody = lens get set
  where get (Vertex body _) = body
        set (Vertex _ children) body = Vertex body children

vertexChildren :: Lens' Vertex (List VertexID)
vertexChildren = lens get set
  where get (Vertex _ children) = children
        set (Vertex body _) children = Vertex body children
