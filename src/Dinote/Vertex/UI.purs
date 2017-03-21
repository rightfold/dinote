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

type State   =
  { vertices :: Map VertexID Vertex
  , pointer  :: VertexID
  }
data Query a = VerticesChanged (Map VertexID Vertex) VertexID a
type Input   = Map VertexID Vertex * VertexID
type Output  = Void

ui :: ∀ m. Component HTML Query Input Output m
ui = component {initialState, render, eval, receiver}
  where
  initialState :: Input -> State
  initialState = uncurry {vertices: _, pointer: _}

  render :: State -> ComponentHTML Query
  render {vertices, pointer} =
    Map.lookup pointer vertices
    # maybe (H.text "This vertex does not exist.")
            (renderBody vertices)

  renderBody :: Map VertexID Vertex -> Vertex -> ComponentHTML Query
  renderBody vertices vertex =
    case vertex ^. vertexBody of
      Left expression ->
        let vertices' = Map.lookup `flip` vertices >>> map (_ ^. vertexBody)
        in H.text (evaluate vertices' expression)
      Right text      -> H.text text

  eval :: Query ~> ComponentDSL State Query Output m
  eval (VerticesChanged vertices pointer next) =
    next <$ State.put {vertices, pointer}

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< uncurry VerticesChanged `flip` unit
