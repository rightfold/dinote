module Dinote.Document
  ( Document(..)
  , documentName
  ) where

import Data.Lens (Lens', lens)

data Document = Document String

documentName :: Lens' Document String
documentName = lens get set
  where get (Document name) = name
        set (Document _) name = Document name
