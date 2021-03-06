-module(unravel_stream_websocket).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([ init/3
        , handle/2
        , terminate/3
        , websocket_handle/3
        , websocket_info/3
        , websocket_init/3
        , websocket_terminate/3
        ]).

-record(state, {}).

%%% * Websocket handler callbacks ==============================================

init(_Type, _Req, __Opts) ->
  {upgrade, protocol, cowboy_websocket}.

handle(Req, State) ->
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

websocket_init(_Type, Req0, _Opts) ->
  {IdBinding, Req} = cowboy_req:binding(id, Req0),
  Id               = binary_to_integer(IdBinding),
  EventStream      = unravel_event_stream:lookup(Id),
  unravel_event_stream:subscribe(EventStream, self()),
  {ok, Req, #state{}}.

websocket_handle(_Msg, Req, State) ->
  {ok, Req, State}.

websocket_info(Info, Req, State) ->
  Msg = case Info of
          {event, {Ms,S,Us}, Pid, Type} ->
            [ {timestamp, Ms * 10.0e6 + S + Us * 10.0e-6}
            , {pid, list_to_binary(pid_to_list(Pid))}
            , {type, Type}
            ];
          X -> lists:flatten(io_lib:format("~p", [X]))
        end,
  Payload = jiffy:encode({Msg}),
  {reply, {text, Payload}, Req, State, hibernate}.

websocket_terminate(_Reason, _, _State) ->
  ok.
