module Dinote.Document
  ( DocumentID(..)
  , Document(..)
  , documentID
  , documentName
  ) where

import Data.Lens (Lens', lens)
import Dinote.Prelude

newtype DocumentID = DocumentID String
derive newtype instance eqDocumentID :: Eq DocumentID
derive newtype instance ordDocumentID :: Ord DocumentID

data Document = Document DocumentID String

documentID :: Lens' Document DocumentID
documentID = lens get set
  where get (Document id _) = id
        set (Document _ name) id = Document id name

documentName :: Lens' Document String
documentName = lens get set
  where get (Document _ name) = name
        set (Document id _) name = Document id name
