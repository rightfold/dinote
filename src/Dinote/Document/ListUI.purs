module Dinote.Document.ListUI
  ( Query
  , Input
  , Output
  , Monad
  , ui
  ) where

import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((^.))
import Dinote.Document (Document, DocumentID, documentID, documentName)
import Dinote.Document.Algebra (DocumentM, getDocuments)
import Dinote.Prelude
import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query (raise)

type State   =
  { documents :: List Document
  , selection :: Maybe DocumentID
  }
data Query a
  = Initialize a
  | Select DocumentID a
type Input   = Unit
type Output  = DocumentID
type Monad   = DocumentM

ui :: Component HTML Query Input Output Monad
ui = lifecycleComponent { initialState
                        , render
                        , eval
                        , receiver
                        , initializer: Just (Initialize unit)
                        , finalizer: Nothing
                        }
  where
  initialState :: Input -> State
  initialState _ = {documents: Nil, selection: Nothing}

  render :: State -> ComponentHTML Query
  render {documents, selection} =
    H.ul [] <<< Array.fromFoldable $
      map (renderDocument selection) documents

  renderDocument :: Maybe DocumentID -> Document -> ComponentHTML Query
  renderDocument selection document =
    H.li [ P.classes classes
         , E.onClick (E.input_ (Select documentID'))
         ]
      [H.text $ document ^. documentName]
    where
    classes = Array.filter (const $ selected) [wrap "-selected"]
    selected = selection == Just documentID'
    documentID' = document ^. documentID

  eval :: Query ~> ComponentDSL State Query Output Monad
  eval (Initialize next) = do
    documents <- lift getDocuments
    State.put {documents, selection: Nothing}
    pure next
  eval (Select id next) = do
    State.modify _ {selection = Just id}
    raise id
    pure next

  receiver :: Input -> Maybe (Query Unit)
  receiver = const Nothing
