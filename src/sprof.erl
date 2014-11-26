-module(sprof).

-export([ apply/2
        , start/1
        , stop/1
        ]).

%%% * API **********************************************************************

apply(Label, Fun) ->
    start_tracing(Label),
    Self = self(),
    Pid = spawn(fun() ->
                        receive trigger ->
                                Result = Fun(),
                                Self ! Result
                        end
                end),
    Pid ! trigger,
    receive
        Result ->
            stop_tracing(),
            {ok, Analysis} = analyse(Label),
            {ok, Result, Analysis}
    after 5000 ->
            timeout
    end.

start(_Options) ->
    Pid = spawn(fun tracer/0),
    seq_trace:set_system_tracer(Pid),
    {ok, Pid}.

stop(Pid) ->
    seq_trace:reset_trace(),
    Pid ! {stop, self()},
    receive
        {stopped, Pid} ->
            ok
    after 5000 ->
            timeout
    end.

%%% * Internal Functions *******************************************************

tracer() ->
    msg_handler([]).

msg_handler(Events0) ->
    receive
        {seq_trace, _, _, _} = Msg ->
            Events = [Msg | Events0],
            msg_handler(Events);
        {dump, From, Label} ->
            Events = lists:filter(fun({seq_trace, L, _, _}) ->
                                          L == Label
                                  end, Events0),
            From ! {reply, lists:reverse(Events)},
             msg_handler([]);
         {stop, From} ->
             From ! {stopped, self()}
     end.

 start_tracing(Label) ->
     seq_trace:set_token(label, Label),
     seq_trace:set_token('receive', true),
     seq_trace:set_token(timestamp, true),
     seq_trace:set_token(send, true).

 stop_tracing() ->
     seq_trace:set_token([]).

 analyse(Label) ->
     Tracer = seq_trace:get_system_tracer(),
     Tracer ! {dump, self(), Label},
     receive
         {reply, Events} ->
             Graph0 = digraph:new(),
             Graph = fold_events(Graph0, Events),
             {ok, Graph}
     after 5000 ->
             timeout
     end.

fold_events(Graph, []) ->
    Graph;
fold_events(Graph, [Event | Events]) ->
    {seq_trace, _Label, TraceInfo, TimeStamp} = Event,
    {Type, {PrevSerial, CurrentSerial}, From, To, Message} = TraceInfo,
    case Type of
        send ->
            Previous = digraph:add_vertex(Graph, {From, PrevSerial}),
            Current  = digraph:add_vertex(Graph, {From, CurrentSerial},
                                          {Type, TimeStamp}),
            digraph:add_edge(Graph, Previous, Current);
        'receive' ->
            Previous = digraph:add_vertex(Graph, {To, PrevSerial}),
            Current  = digraph:add_vertex(Graph, {To, CurrentSerial},
                                          {Type, TimeStamp}),
            digraph:add_edge(Graph, Previous, Current),

            Source = find_sender(Graph, digraph:vertices(Graph), From, Message),
            digraph:add_edge(Graph, Source, Current, Message);
        _   -> ok
    end,
    fold_events(Graph, Events).

find_sender(Graph, [], From, Message) ->
    digraph:add_vertex(Graph, {From, 0}, {send, unknown, Message});
find_sender(Graph, [Vertex | Vertices], From, Message) ->
    case digraph:vertex(Graph, Vertex) of
        {{From, _}, {send, _, Message}} ->
            Vertex;
        _ -> find_sender(Graph, Vertices, From, Message)
    end.
