module Dinote.Vertex.UI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Control.Monad.State.Class as State
import Data.Lens (Lens', (.=), (^.), lens)
import Data.StrMap as StrMap
import Dinote.Prelude
import Dinote.Vertex (Vertex, vertexBody)
import DOM.Event.KeyboardEvent as KeyboardEvent
import DOM.Event.KeyboardEvent (altKey, ctrlKey, shiftKey)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query (raise)
import Text.Markdown.SlamDown.Halogen.Component (SlamDownState(..), renderSlamDown)
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Syntax (SlamDownP)

type State   =
  { vertex  :: Vertex
  , editing :: Boolean
  }
data Query a
  = Void a
  | VertexChanged Vertex a
  | BeginEdit a
  | SaveEdit String a
  | CommitEdit a
type Input   = Vertex
type Output  = Vertex

stateVertex :: Lens' State Vertex
stateVertex = lens _.vertex _ {vertex = _}

stateEditing :: Lens' State Boolean
stateEditing = lens _.editing _ {editing = _}

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
    H.textarea
      [ E.onValueChange (E.input SaveEdit)
      , E.onBlur (E.input_ CommitEdit)
      , E.onKeyDown ((*>) <$> guard <<< commitKey <*> E.input_ CommitEdit)
      , P.value textual
      ]
    where
    commitKey = (&&) <$> eq "Enter" <<< KeyboardEvent.key
                     <*> not (altKey || ctrlKey || shiftKey)
    textual = vertex ^. vertexBody

  renderViewer :: Vertex -> ComponentHTML Query
  renderViewer vertex =
    H.div [E.onDoubleClick (E.input_ BeginEdit)] $
      case parseMd (vertex ^. vertexBody) of
        Left error -> [H.text error]
        Right (document :: SlamDownP String) ->
          let sdState = SlamDownState {document, formState: StrMap.empty}
          in [Void unit <$ renderSlamDown sdConfig sdState]
    where
    sdConfig = { formName: ""
               , browserFeatures: {inputTypeSupported: const false}
               }

  eval :: Query ~> ComponentDSL State Query Output m
  eval (Void next) = pure next
  eval (VertexChanged vertex next) = next <$ (stateVertex .= vertex)
  eval (BeginEdit next) = next <$ (stateEditing .= true)
  eval (SaveEdit text next) = next <$ (stateVertex <<< vertexBody .= text)
  eval (CommitEdit next) = do
    stateEditing .= false
    raise =<< State.gets _.vertex
    pure next

  receiver :: Input -> Maybe (Query Unit)
  receiver = Just <<< VertexChanged `flip` unit
