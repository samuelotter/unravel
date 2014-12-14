-module(unravel_web).

-export([ start/0
        , open/1
        , stop/0
        ]).

start() ->
  Routes = [ {'_', [ {"/", cowboy_static, { priv_file, unravel
                                          , "www/index.html"}}
                   , {"/:id/stream", unravel_stream_websocket, []}
                   , {"/:id", cowboy_static, { priv_file, unravel
                                          , "www/index.html"}}
                   , {"/js/unravel.js", cowboy_static, { priv_file, unravel
                                                       , "elm/js/unravel.js"}}
                   ]}
           ],
  Dispatch = cowboy_router:compile(Routes),
  %% FIXME: Move to app?
  {ok, _} = application:ensure_all_started(unravel),
  cowboy:start_http(unravel_http_listener, 100, [{port, 8888}],
                    [{env, [{dispatch, Dispatch}]}]).

open(Filename) ->
  %% FIXME: Better id-generation?
  Id = erlang:phash2(Filename),
  {ok, Stream} = unravel_event_stream:start(Id),
  unravel_trace_client:open_file(Filename, Stream),
  io:format("Open http://localhost:8888/~w~n", [Id]),
  Id.

stop() ->
  cowboy:stop_listener(unravel_http_listener).
