module Dinote.Vertex.UI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Control.Monad.State.Class as State
import Data.Lens ((^.))
import Data.Map as Map
import Dinote.Expression.Evaluate (evaluate)
import Dinote.Prelude
import Dinote.Vertex (Vertex, VertexID, vertexBody)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

type State   =
  { vertices :: Map VertexID Vertex
  , pointer  :: VertexID
  , editing  :: Boolean
  }
data Query a
  = VerticesChanged (Map VertexID Vertex) VertexID a
  | BeginEdit a
type Input   = Map VertexID Vertex * VertexID
type Output  = VertexID * Vertex

ui :: ∀ m. Component HTML Query Input Output m
ui = component {initialState, render, eval, receiver}
  where
  initialState :: Input -> State
  initialState = uncurry {vertices: _, pointer: _, editing: false}

  render :: State -> ComponentHTML Query
  render {vertices, pointer, editing} =
    Map.lookup pointer vertices
    # maybe (H.text "This vertex does not exist.")
            ((if editing then renderEditor else renderViewer) vertices)

  renderEditor :: Map VertexID Vertex -> Vertex -> ComponentHTML Query
  renderEditor vertices vertex = H.input [P.value textual]
    where
    textual = case vertex ^. vertexBody of
      Left expression -> "(todo)"
      Right text      -> text

  renderViewer :: Map VertexID Vertex -> Vertex -> ComponentHTML Query
  renderViewer vertices vertex =
    H.div [E.onDoubleClick (E.input_ BeginEdit)]
      [go]
    where
    go = case vertex ^. vertexBody of
      Left expression ->
        let vertices' = Map.lookup `flip` vertices >>> map (_ ^. vertexBody)
        in H.text (evaluate vertices' expression)
      Right text      -> H.text text

  eval :: Query ~> ComponentDSL State Query Output m
  eval (VerticesChanged vertices pointer next) =
    next <$ State.modify _ {vertices = vertices, pointer = pointer}
  eval (BeginEdit next) = next <$ State.modify _ {editing = true}

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< uncurry VerticesChanged `flip` unit
