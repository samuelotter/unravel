module Unravel where

import Graphics.Element (..)
import Html (..)
import List (..)
import Signal
import String
import Text
import WebSocket
import Window

main = inputs
       |> state
       |> Signal.map view
       |> Signal.map2 render Window.dimensions

render : (Int, Int) -> Html -> Element
render (w, h) html = toElement w h html

type alias Event = String
type alias State = List Event

update : Input -> State -> State
update input state = case input of
                       ServerMessage msg -> msg :: state

view : State -> Html
view state = map (\str -> p [] [text str]) state
             |> div []

state : Signal Input -> Signal State
state input = Signal.foldp update [] input

type Input = ServerMessage String

server = Signal.channel ""

inputs : Signal Input
inputs = Signal.map ServerMessage (stream (Signal.subscribe server))

stream input =
    let base = case location of
                 Just str -> String.dropLeft 4 str
                 Nothing  -> ""
    in
      WebSocket.connect ("ws" ++ base ++ "/stream") input

port location : Maybe String