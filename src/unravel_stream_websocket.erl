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

handle(_Req, State) ->
  {noreply, State}.

terminate(_Reason, _Req, _State) ->
  ok.

websocket_init(_Type, Req, _Opts) ->
  %% TODO: Setup subscription to event stream.
  {ok, Req, #state{}}.

websocket_handle(_Msg, Req, State) ->
  {reply, {text, <<"Init">>}, Req, State, hibernate}.

websocket_info(Info, Req, State) ->
  {reply, {text, list_to_binary(Info)}, Req, State, hibernate}.

websocket_terminate(_Reason, _, _State) ->
  ok.
