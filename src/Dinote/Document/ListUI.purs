module Dinote.Document.ListUI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((^.))
import Halogen.Component (Component, ComponentDSL, ComponentHTML, lifecycleComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Dinote.Document (Document, documentName)
import Dinote.Document.Algebra (DocumentM, getDocuments)
import Dinote.Prelude

type State   = List Document
data Query a = Initialize a
type Input   = Unit
type Output  = Void

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
  initialState _ = Nil

  render :: State -> ComponentHTML Query
  render documents = H.ul [] <<< Array.fromFoldable $
    map renderDocument documents

  renderDocument :: Document -> ComponentHTML Query
  renderDocument document = H.li [] [H.text $ document ^. documentName]

  eval :: Query ~> ComponentDSL State Query Output DocumentM
  eval (Initialize next) = next <$ (State.put =<< lift getDocuments)

  receiver :: Input -> Maybe (Query Unit)
  receiver = const Nothing
