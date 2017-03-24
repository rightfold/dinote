module Dinote.Vertex.UI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Control.Monad.State.Class as State
import Data.Lens ((.~), (^.))
import Dinote.Expression as Expression
import Dinote.Prelude
import Dinote.Vertex (Vertex, vertexBody)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query (raise)

type State   =
  { vertex  :: Vertex
  , editing :: Boolean
  }
data Query a
  = VertexChanged Vertex a
  | BeginEdit a
  | CommitEdit String a
type Input   = Vertex
type Output  = Vertex

ui :: ∀ m. Component HTML Query Input Output m
ui = component {initialState, render, eval, receiver}
  where
  initialState :: Input -> State
  initialState = {vertex: _, editing: false}

  render :: State -> ComponentHTML Query
  render {vertex, editing: true}  = renderEditor vertex
  render {vertex, editing: false} = renderViewer vertex

  renderEditor :: Vertex -> ComponentHTML Query
  renderEditor vertex =
    H.input [ E.onValueChange (E.input CommitEdit)
            , P.value textual
            ]
    where
    textual = case vertex ^. vertexBody of
      Left expression -> "=" <> Expression.pretty expression
      Right text      -> text

  renderViewer :: Vertex -> ComponentHTML Query
  renderViewer vertex =
    H.div [E.onDoubleClick (E.input_ BeginEdit)]
      [go]
    where
    go = case vertex ^. vertexBody of
      Left expression -> H.text "TODO"
      Right text      -> H.text text

  eval :: Query ~> ComponentDSL State Query Output m
  eval (VertexChanged vertex next) = next <$ State.modify _ {vertex = vertex}
  eval (BeginEdit next) = next <$ State.modify _ {editing = true}
  eval (CommitEdit text next) = do
    State.modify \s ->
      let vertex' = s.vertex # vertexBody .~ Right text
      in s {vertex = vertex', editing = false}
    raise =<< State.gets _.vertex
    pure next

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< VertexChanged `flip` unit
