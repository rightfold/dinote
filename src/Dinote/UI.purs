module Dinote.UI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Halogen.Component (Component, ParentDSL, ParentHTML, parentComponent)
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Dinote.Document (DocumentID)
import Dinote.Document.ListUI as Document.ListUI
import Dinote.Prelude

type State      = Unit
data Query a    = DocumentSelected DocumentID a
type ChildQuery = Document.ListUI.Query <+> Const Void
type Input      = Unit
type Output     = Void
type Slot       = Unit + Void
type Monad      = Document.ListUI.Monad

ui :: Component HTML Query Input Output Monad
ui = parentComponent {initialState, render, eval, receiver}
  where
  initialState :: Input -> State
  initialState _ = unit

  render :: State -> ParentHTML Query ChildQuery Slot Monad
  render _ = H.slot' cp1 unit Document.ListUI.ui unit handle
    where handle = Just <<< DocumentSelected `flip` unit

  eval :: Query ~> ParentDSL State Query ChildQuery Slot Output Monad
  eval (DocumentSelected id next) = next <$ traceAnyA id

  receiver :: Input -> Maybe (Query Unit)
  receiver = const Nothing
