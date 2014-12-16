module Unravel where

import Debug
import Dict
import Graphics.Element (..)
import Html (..)
import Json.Decode (..)
import List (..)
import List
import Maybe (..)
import Result
import Signal
import String
import Text
import WebSocket
import Window

type Input = ServerMessage (Maybe Event)

type alias Event = { timestamp : Float, pid: String, eventType: String }
type alias State = { processes: Dict.Dict String (List Event) }


main = inputs
       |> state
       |> Signal.map view
       |> Signal.map2 render Window.dimensions

render : (Int, Int) -> Html -> Element
render (w, h) html = toElement w h html

update : Input -> State -> State
update input state =
    case input of
      ServerMessage (Just event) ->
          let processes =
                  case Dict.get event.pid state.processes of
                    Just events ->
                        Dict.insert event.pid (event :: events) state.processes
                    Nothing ->
                        Dict.insert event.pid [event] state.processes
          in
            { state | processes <- processes }
      _ ->
          Debug.log "Unhandled message" state

view : State -> Html
view state =
    Dict.toList state.processes
        |> List.map processView
        |> div []

processView : (String,  (List Event)) -> Html
processView (pid, events) =
    div [] ((text pid) :: List.map (.eventType >> text) events)

initialState = { processes = Dict.empty }

state : Signal Input -> Signal State
state input = Signal.foldp update initialState input


server = Signal.channel ""

inputs : Signal Input
inputs = Signal.map ServerMessage (stream (Signal.subscribe server))

stream input =
    let base = case location of
                 Just str -> String.dropLeft 4 str
                 Nothing  -> ""
     in
      WebSocket.connect ("ws" ++ base ++ "/stream") input
          |> Signal.map (decodeString eventDecoder)
          |> Signal.map Result.toMaybe


eventDecoder =
    object3 Event
                ("timestamp" := float)
                ("pid"       := string)
                ("type"      := string)


port location : Maybe String
