module Dinote.Document
  ( DocumentID(..)
  , Document(..)
  , documentName
  ) where

import Data.Lens (Lens', lens)
import Dinote.Prelude

newtype DocumentID = DocumentID String
derive newtype instance eqDocumentID :: Eq DocumentID
derive newtype instance ordDocumentID :: Ord DocumentID

data Document = Document String

documentName :: Lens' Document String
documentName = lens get set
  where get (Document name) = name
        set (Document _) name = Document name
