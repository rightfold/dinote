module Dinote.Document.EditUI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Control.Monad.State.Class as State
import Dinote.Document (DocumentID(..))
import Dinote.Prelude
import Halogen.Component (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML)
import Halogen.HTML as H

type State   = DocumentID
data Query a = DocumentSelected DocumentID a
type Input   = DocumentID
type Output  = Void

ui :: ∀ m. Component HTML Query Input Output m
ui = component {initialState, render, eval, receiver}
  where
  initialState :: Input -> State
  initialState = id

  render :: State -> ComponentHTML Query
  render (DocumentID id) =
    H.text $ "You have selected document " <> id

  eval :: Query ~> ComponentDSL State Query Output m
  eval (DocumentSelected id next) =
    next <$ State.put id

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< DocumentSelected `flip` unit
