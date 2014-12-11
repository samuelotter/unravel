-module(unravel_trace_client).

-export([ open_file/2 ]).

open_file(Filename, Stream) ->
  Handler = fun(end_of_trace, _) ->
                ok;
               (TraceTs, _) ->
                Event = unravel_event:from_trace_ts(TraceTs),
                unravel_event_stream:publish(Stream, Event)
            end,
  dbg:trace_client(file, Filename, {Handler, []}).
