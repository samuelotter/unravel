-module(unravel_event_table).

-include("unravel_event.hrl").

-export([ insert/2
        , new/0
        , select/3
        , continue/1
        ]).

%%% * Types ====================================================================

-record(event_table, {table :: ets:tab()}).

-opaque event_table() :: #event_table{}.

-export_type([ event_table/0 ]).

%%% * API ======================================================================

-spec insert(Event :: unravel_event:event(), Table :: event_table()) -> ok.
insert(Event, Table) ->
  ets:insert(Table#event_table.table, Event).

-spec new() -> event_table().
new() ->
  #event_table{table = ets:new(events, [ordered_set, {keypos, 2}, public])}.

select(Start, End, Stream) ->
  MatchSpec = [{ #event{timestamp = $1, _ = '_'}
               , [{ 'and'
                  , {'>=', '$1', Start}
                  , {'<', '$1', End}}]
               , ['$_']}],
  ets:select(Stream#event_table.table, MatchSpec, 100).

continue(Continuation) ->
  ets:select(Continuation).
