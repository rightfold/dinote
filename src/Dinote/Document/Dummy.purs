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
runDocumentF (GetDocument id next) = pure <<< next $ case id of
  DocumentID "1" -> Just $ Document "Lorem" Map.empty
  DocumentID "2" -> Just $ Document "Ipsum" Map.empty
  _ -> Nothing
