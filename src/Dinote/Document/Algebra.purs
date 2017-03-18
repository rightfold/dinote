module Dinote.Document.Algebra
  ( DocumentM
  , DocumentF(..)
  , getDocuments
  ) where

import Dinote.Document (Document)
import Dinote.Prelude

type DocumentM = Free DocumentF

data DocumentF a
  = GetDocuments (List Document -> a)

getDocuments :: DocumentM (List Document)
getDocuments = liftF $ GetDocuments id
