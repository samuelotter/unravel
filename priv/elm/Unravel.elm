module Unravel where

import Html (..)
import List (..)
import Signal
import Text
import WebSocket

main = inputs "profile"
       |> state
       |> Signal.map view
       |> Signal.map (toElement 400 400)

type alias Event = String
type alias State = List Event

update : Input -> State -> State
update input state = case input of
                       ServerMessage msg -> msg :: state

view : State -> Html
view state = map (\str -> text str) state
             |> div []

state : Signal Input -> Signal State
state input = Signal.foldp update [] input

type Input = ServerMessage String

server = Signal.channel ""

inputs : String -> Signal Input
inputs streamId = Signal.map ServerMessage (stream streamId (Signal.subscribe server))

stream id input = WebSocket.connect ("ws://localhost:8888/stream/" ++ id) input