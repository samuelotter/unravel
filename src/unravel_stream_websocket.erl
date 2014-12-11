-module(unravel_stream_websocket).

-export([ init/2
        , terminate/3
        , websocket_handle/3
        , websocket_info/3
        ]).

-record(state, {}).

%%% * Websocket handler callbacks ==============================================

init(Req, _Opts) ->

  {cowboy_websocket, Req, #state{}}.

terminate(_Reason, _Req, _State) ->
  ok.

websocket_handle(_Msg, _Req, _State) ->
  ok.

websocket_info(_Info, _Req, _State) ->
  ok.
