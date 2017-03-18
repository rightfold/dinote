module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Dinote.UI (ui)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude

main :: ∀ eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI ui unit
