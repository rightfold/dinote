module Dinote.Document.Dummy
  ( runDocumentF
  ) where

import Data.Map as Map
import Dinote.Document (Document(..), DocumentID(..))
import Dinote.Document.Algebra (DocumentF(..))
import Dinote.Prelude

runDocumentF :: ∀ m. (Applicative m) => DocumentF ~> m
runDocumentF (GetDocuments next) = pure <<< next $
  Map.empty
  # Map.insert (DocumentID "1") (Document "Lorem" unit)
  # Map.insert (DocumentID "2") (Document "Ipsum" unit)
