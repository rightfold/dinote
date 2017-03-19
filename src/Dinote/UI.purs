module Dinote.UI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Control.Monad.State.Class as State
import Dinote.Document (DocumentID)
import Dinote.Document.Algebra (DocumentM)
import Dinote.Document.EditUI as Document.EditUI
import Dinote.Document.ListUI as Document.ListUI
import Dinote.Prelude
import Halogen.Component (Component, ParentDSL, ParentHTML, parentComponent)
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML (HTML)
import Halogen.HTML as H

type State      = Maybe DocumentID
data Query a    = DocumentSelected DocumentID a
type ChildQuery = Document.ListUI.Query <+> Document.EditUI.Query <+> Const Void
type Input      = Unit
type Output     = Void
type Slot       = Unit + Unit + Void
type Monad      = DocumentM

ui :: Component HTML Query Input Output Monad
ui = parentComponent {initialState, render, eval, receiver}
  where
  initialState :: Input -> State
  initialState _ = Nothing

  render :: State -> ParentHTML Query ChildQuery Slot Monad
  render selection =
    H.div []
      [ H.slot' cp1 unit Document.ListUI.ui unit handle
      , case selection of
          Nothing -> H.text "Please select a document."
          Just id -> H.slot' cp2 unit Document.EditUI.ui id absurd
      ]
    where handle = Just <<< DocumentSelected `flip` unit

  eval :: Query ~> ParentDSL State Query ChildQuery Slot Output Monad
  eval (DocumentSelected id next) =
    next <$ State.put (Just id)

  receiver :: Input -> Maybe (Query Unit)
  receiver = const Nothing
