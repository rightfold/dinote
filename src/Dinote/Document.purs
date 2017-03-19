module Dinote.Document
  ( DocumentID(..)
  , Document(..)
  , documentName
  , documentBody
  ) where

import Data.Lens (Lens', lens)
import Dinote.Prelude

newtype DocumentID = DocumentID String
derive newtype instance eqDocumentID :: Eq DocumentID
derive newtype instance ordDocumentID :: Ord DocumentID

data Document a = Document String a

documentName :: ∀ a. Lens' (Document a) String
documentName = lens get set
  where get (Document name _) = name
        set (Document _ body) name = Document name body

documentBody :: ∀ a. Lens' (Document a) a
documentBody = lens get set
  where get (Document _ body) = body
        set (Document name _) body = Document name body
