module Dinote.Document.Algebra
  ( DocumentM
  , DocumentF(..)
  , getDocuments
  , getDocument
  ) where

import Dinote.Document (Document, DocumentID)
import Dinote.Prelude
import Dinote.Vertex (Vertex, VertexID)

type DocumentM = Free DocumentF

data DocumentF a
  = GetDocuments (Map DocumentID (Document Unit) -> a)
  | GetDocument DocumentID (Maybe (Document (Map VertexID Vertex)) -> a)

getDocuments :: DocumentM (Map DocumentID (Document Unit))
getDocuments = liftF $ GetDocuments id

getDocument :: DocumentID -> DocumentM (Maybe (Document (Map VertexID Vertex)))
getDocument id = liftF $ GetDocument id (const `flip` unit)
