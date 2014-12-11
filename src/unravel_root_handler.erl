-module(unravel_root_handler).
-behaviour(cowboy_http_handler).

%%% Http callbacks
-export([ init/3
        , handle/2
        , terminate/3
        ]).

-record(state, {
         }).

%%% * Http Handler callbacks ===================================================

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
