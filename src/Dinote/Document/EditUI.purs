module Dinote.Document.EditUI
  ( Query
  , Input
  , Output
  , Monad
  , ui
  ) where

import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((.=), (^.), first, second, use)
import Data.Map as Map
import Dinote.Document (Document, DocumentID, documentBody)
import Dinote.Document.Algebra (DocumentM, getDocument)
import Dinote.Prelude
import Dinote.Vertex (Vertex, VertexID)
import Dinote.Vertex.UI as Vertex.UI
import Halogen.Component (Component, ParentDSL, ParentHTML, lifecycleParentComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H

type State      = DocumentID * Maybe (Document (Map VertexID Vertex))
data Query a
  = Initialize a
  | DocumentSelected DocumentID a
type ChildQuery = Vertex.UI.Query
type Input      = DocumentID
type Output     = Void
type Slot       = VertexID
type Monad      = DocumentM

ui :: Component HTML Query Input Output Monad
ui = lifecycleParentComponent { initialState
                              , render
                              , eval
                              , receiver
                              , initializer: Just (Initialize unit)
                              , finalizer: Nothing
                              }
  where
  initialState :: Input -> State
  initialState = (_ /\ Nothing)

  render :: State -> ParentHTML Query ChildQuery Slot Monad
  render (_ /\ Nothing) = H.text "This document does not exist."
  render (_ /\ Just document) =
    H.ul [] $
      vertices
      # Map.keys
      # Array.fromFoldable
      # map (H.li [] <<< pure <<< renderVertex vertices)
    where vertices = document ^. documentBody

  renderVertex
    :: Map VertexID Vertex
    -> VertexID
    -> ParentHTML Query ChildQuery Slot Monad
  renderVertex vertices vertexID =
    H.slot vertexID Vertex.UI.ui (vertices /\ vertexID) absurd

  eval :: Query ~> ParentDSL State Query ChildQuery Slot Output Monad
  eval (Initialize next) = reload next
  eval (DocumentSelected id next) = State.put (id /\ Nothing) *> reload next

  reload :: ∀ a. a -> ParentDSL State Query ChildQuery Slot Output Monad a
  reload = (_ <$ (second .= _) =<< lift <<< getDocument =<< use first)

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< DocumentSelected `flip` unit
