module Dinote.Document.EditUI
  ( Query
  , Input
  , Output
  , Monad
  , ui
  ) where

import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.Lens ((.=), (^.), _Just, first, second, use)
import Data.Lens.Index (ix)
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
  | VertexChanged VertexID Vertex a
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
      document ^. documentBody
      # Map.toUnfoldable
      # map (H.li [] <<< pure <<< uncurry renderVertex)

  renderVertex
    :: VertexID
    -> Vertex
    -> ParentHTML Query ChildQuery Slot Monad
  renderVertex vertexID vertex =
    H.slot vertexID Vertex.UI.ui vertex handle
    where handle = Just <<< VertexChanged vertexID `flip` unit

  eval :: Query ~> ParentDSL State Query ChildQuery Slot Output Monad
  eval (Initialize next) = reload next
  eval (DocumentSelected id next) = State.put (id /\ Nothing) *> reload next
  eval (VertexChanged vertexID vertex next) = do
    second <<< _Just <<< documentBody <<< ix vertexID .= vertex
    pure next

  reload :: ∀ a. a -> ParentDSL State Query ChildQuery Slot Output Monad a
  reload = (_ <$ (second .= _) =<< lift <<< getDocument =<< use first)

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< DocumentSelected `flip` unit
