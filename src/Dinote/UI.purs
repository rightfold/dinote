module Dinote.UI
  ( Query
  , Input
  , Output
  , ui
  ) where

import Halogen.Component (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Dinote.Prelude

type State   = Unit
data Query a = Query Void
type Input   = Unit
type Output  = Void

ui :: ∀ m. Component HTML Query Input Output m
ui = component {initialState, render, eval, receiver}
  where
  initialState :: Input -> State
  initialState _ = unit

  render :: State -> ComponentHTML Query
  render _ = H.text "hello"

  eval :: Query ~> ComponentDSL State Query Output m
  eval (Query v) = absurd v

  receiver :: Input -> Maybe (Query Unit)
  receiver = const Nothing
