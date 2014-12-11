-module(unravel_trace).

-export([ start/1
        , stop/0
        ]).

start(Filename) ->
    Pid = spawn(fun() ->
                    trace_server(Filename)
                end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

trace_server(Filename) ->
    start_tracing(Filename),
    receive stop ->
            ok
    after 5000 ->
            ok
    end,
    stop_tracing().

start_tracing(Filename) ->
    io:format("starting tracing~n"),
    Tracer = open_trace_port(file, Filename),
    TraceOptions = [ send, 'receive', running, exiting, garbage_collection
                   , timestamp, {tracer, Tracer}
                   ],
    erlang:trace(all, true, TraceOptions).

stop_tracing() ->
    io:format("stopped tracing~n"),
    erlang:trace(all, false, [all]),
    dbg:flush_trace_port().

open_trace_port(Type, Spec) ->
    Fun = dbg:trace_port(Type, Spec),
    Fun().
