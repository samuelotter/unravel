-module(unravel_event).

-export([ from_trace_ts/1
        , new/0
        ]).

-include("unravel_event.hrl").

%%% * Types ====================================================================

-type timestamp() :: {pos_integer(), pos_integer(), pos_integer()}.
-type event()     :: #event{}.

-export_type([ event/0
             ]).

%%% * API ======================================================================

-spec new() -> event().
new() ->
    #event{}.

-spec from_trace_ts({trace_ts, pid(), atom(), any(), timestamp()}) ->
                       event();
                   ({trace_ts, pid(), atom(), any(), pid(), timestamp()}) ->
                       event().
from_trace_ts({trace_ts, Pid, Type, _Data, Ts}) ->
    #event{ timestamp = Ts
          , pid = Pid
          , type = Type
          };
from_trace_ts({trace_ts, Pid, Type, _Data, _X, Ts}) ->
    #event{ timestamp = Ts
          , pid = Pid
          , type = Type
          }.
