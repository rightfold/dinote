module Dinote.Document.Dummy
  ( runDocumentF
  ) where

import Dinote.Document (Document(..), DocumentID(..))
import Dinote.Document.Algebra (DocumentF(..))
import Dinote.Prelude

runDocumentF :: ∀ m. (Applicative m) => DocumentF ~> m
runDocumentF (GetDocuments next) = pure <<< next $
  ( Document (DocumentID "1") "Lorem"
  : Document (DocumentID "2") "Ipsum"
  : Nil
  )
