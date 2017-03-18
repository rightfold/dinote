module Dinote.Document.Dummy
  ( runDocumentF
  ) where

import Dinote.Document (Document(..))
import Dinote.Document.Algebra (DocumentF(..))
import Dinote.Prelude

runDocumentF :: ∀ m. (Applicative m) => DocumentF ~> m
runDocumentF (GetDocuments next) =
  pure $ next (Document "Lorem" : Document "Ipsum" : Nil)
