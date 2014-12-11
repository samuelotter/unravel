-module(unravel_event_stream).
-behaviour(gen_server).

%% API
-export([ publish/2
        , start/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {table = undefined}).

%%% * API ======================================================================

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start(?MODULE, [], []).

-spec publish(Pid :: pid(), Event :: unravel_event:event()) -> ok.
publish(Pid, Event) ->
    gen_server:cast(Pid, {publish, Event}).

%%% * Callbacks ================================================================

init([]) ->
    {ok, #state{table = unravel_event_table:new()}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({publish, Event}, State) ->
    unravel_event_table:insert(Event, State#state.table),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
