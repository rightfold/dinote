module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Dinote.Document.Dummy (runDocumentF)
import Dinote.Prelude
import Dinote.UI (ui)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Component (hoist)
import Halogen.VDom.Driver (runUI)

main :: ∀ eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI (hoist run ui) unit
  where run = foldFree runDocumentF
