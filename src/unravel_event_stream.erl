-module(unravel_event_stream).
-behaviour(gen_server).

%% API
-export([ lookup/1
        , publish/2
        , start/1
        , subscribe/2
        , unsubscribe/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, { table = undefined
               , subscriptions = []
               }).

%%% * API ======================================================================

-spec start(Id :: any()) -> {ok, pid()}.
start(Id) ->
  gen_server:start(?MODULE, [Id], []).

-spec lookup(Id :: any()) -> pid() | undefined.
lookup(Id) ->
  gproc:where({n, l, {?MODULE, Id}}).

-spec publish(Pid :: pid(), Event :: unravel_event:event()) -> ok.
publish(Pid, Event) ->
  gen_server:cast(Pid, {publish, Event}).

-spec subscribe(Pid :: pid(), Subscriber :: pid()) -> ok.
subscribe(Pid, Subscriber) ->
  gen_server:call(Pid, {subscribe, Subscriber}).

-spec unsubscribe(Pid :: pid(), Subscriber :: pid()) -> ok.
unsubscribe(Pid, Subscriber) ->
  gen_server:call(Pid, {unsubscribe, Subscriber}).

%%% * Callbacks ================================================================

init([Id]) ->
  gproc:reg({n, l, {?MODULE, Id}}, []),
  {ok, #state{table = unravel_event_table:new()}}.

handle_call({subscribe, Subscriber}, _From, State) ->
  Subscriptions = [Subscriber | State#state.subscriptions],
  spawn_link(fun () ->
                 Events = unravel_event_table:to_list(State#state.table),
                 lists:foreach(fun(Event) -> Subscriber ! Event end, Events)
             end),
  {reply, ok, State#state{subscriptions = Subscriptions}};
handle_call({unsubscribe, Subscriber}, _From, State) ->
  Subscriptions = lists:delete(Subscriber, State#state.subscriptions),
  {reply, ok, State#state{subscriptions = Subscriptions}}.

handle_cast({publish, Event}, State) ->
    unravel_event_table:insert(Event, State#state.table),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
