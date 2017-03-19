module Dinote.Document.Algebra
  ( DocumentM
  , DocumentF(..)
  , getDocuments
  ) where

import Dinote.Document (Document, DocumentID)
import Dinote.Prelude

type DocumentM = Free DocumentF

data DocumentF a
  = GetDocuments (Map DocumentID (Document Unit) -> a)

getDocuments :: DocumentM (Map DocumentID (Document Unit))
getDocuments = liftF $ GetDocuments id
