module Dinote.Document.EditUI
  ( Query
  , Input
  , Output
  , Monad
  , ui
  ) where

import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.Lens ((.=), first, second, use)
import Dinote.Document (Document, DocumentID(..))
import Dinote.Document.Algebra (DocumentM, getDocument)
import Dinote.Prelude
import Dinote.Vertex (Vertex, VertexID)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H

type State   = DocumentID * Maybe (Document (Map VertexID Vertex))
data Query a
  = Initialize a
  | DocumentSelected DocumentID a
type Input   = DocumentID
type Output  = Void
type Monad   = DocumentM

ui :: Component HTML Query Input Output DocumentM
ui = lifecycleComponent { initialState
                        , render
                        , eval
                        , receiver
                        , initializer: Just (Initialize unit)
                        , finalizer: Nothing
                        }
  where
  initialState :: Input -> State
  initialState = (_ /\ Nothing)

  render :: State -> ComponentHTML Query
  render (_ /\ Nothing) = H.text "This document does not exist."
  render (_ /\ Just document) = H.text "You have selected a document!"

  eval :: Query ~> ComponentDSL State Query Output DocumentM
  eval (Initialize next) = reload next
  eval (DocumentSelected id next) = State.put (id /\ Nothing) *> reload next

  reload :: ∀ a. a -> ComponentDSL State Query Output DocumentM a
  reload = (_ <$ (second .= _) =<< lift <<< getDocument =<< use first)

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< DocumentSelected `flip` unit
