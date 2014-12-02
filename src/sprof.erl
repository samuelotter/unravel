-module(sprof).

-export([ apply/2
        , export/3
        , start/1
        , stop/0
        , run_test/0
        , print_graph/1
        , player/2
        ]).

-record(event, {serial, pid, timestamp, type}).
-record(send_event, {to, message}).
-record(receive_event, {from, message}).

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
    Result = receive R  -> R
             after 5000 -> timeout
             end,
    stop_tracing(),
    {ok, Analysis} = analyse(Label),
    {ok, Result, Analysis}.

start(_Options) ->
    Pid = spawn(fun tracer/0),
    seq_trace:set_system_tracer(Pid),
    {ok, Pid}.

stop() ->
    seq_trace:reset_trace(),
    Pid = seq_trace:get_system_tracer(),
    Pid ! {stop, self()},
    receive
        {stopped, Pid} ->
            ok
    after 5000 ->
            timeout
    end.

run_test() ->
    sprof:start([]),
    {ok, _, Graph} = sprof:apply(123, fun test/0),
    sprof:export(dot, "test.dot", Graph),
    sprof:stop().

player(Controller, 0) ->
    Controller ! {finished, self()};
player(Controller, Count) ->
    receive
        {Msg, From} ->
            From ! {Msg, self()},
            player(Controller, Count - 1)
    end.

test() ->
    Rounds = 10,
    A = spawn(?MODULE, player, [self(), Rounds]),
    B = spawn(?MODULE, player, [self(), Rounds]),
    B ! {payload, A},
    receive {finished, _} -> ok end,
    receive {finished, _} -> ok end.

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
            SortedEvents = lists:sort(Events),
            Graph1 = lists:foldl(fun create_vertex/2, Graph0, SortedEvents),
            Graph  = lists:foldl(fun create_edge/2, Graph1, SortedEvents),
            io:format("~p~n", [SortedEvents]),
            {ok, Graph}
    after 5000 ->
            timeout
    end.

create_vertex({seq_trace, _Label, TraceInfo, TimeStamp}, Graph) ->
    {Type, Serial, From, To, Message} = TraceInfo,
    case Type of
        'send' ->
            Event = #event{ serial    = Serial
                          , pid       = From
                          , timestamp = TimeStamp
                          , type      = #send_event{ to = To
                                                   , message = Message
                                                   }},
            vertex(Graph, {Serial, send, From}, Event);
        'receive' ->
            Event = #event{ serial    = Serial
                          , pid       = To
                          , timestamp = TimeStamp
                          , type      = #receive_event{ from = From
                                                      , message = Message
                                                      }},
            vertex(Graph, {Serial, 'receive', To}, Event);
        _ -> ok
    end,
    Graph.

create_edge({seq_trace, _Label, TraceInfo, _TimeStamp}, Graph) ->
    {_Type, Serial, From, To, Message} = TraceInfo,
    V1 = {Serial, send, From},
    V2 = {Serial, 'receive', To},
    case digraph:get_short_path(Graph, V1, V2) of
        false ->
            digraph:add_edge(Graph, V1, V2, {Serial, Message}),
            Graph;
        _Path ->
            Graph
    end.

vertex(Graph, Vertex, Label) ->
    case digraph:vertex(Graph, Vertex) of
        {Vertex, Label} ->
            {Vertex, Label};
        {Vertex, []} ->
            digraph:add_vertex(Graph, Vertex, Label);
        {Vertex, ExistingLabel} ->
            {Vertex, ExistingLabel};
        false ->
            digraph:add_vertex(Graph, Vertex, Label)
    end.

print_graph(Graph) ->
    Edges = digraph:edges(Graph),
    lists:foreach(fun(EdgeRef) ->
                          {EdgeRef, From, To, Message} =
                              digraph:edge(Graph, EdgeRef),
                          FromVertex = digraph:vertex(Graph, From),
                          ToVertex   = digraph:vertex(Graph, To),
                          io:format("~p --[ ~p ]--> ~p~n",
                                    [FromVertex, Message, ToVertex])
                  end, Edges).

export(dot, Filename, Graph) ->
    {ok, File} = file:open(Filename, [write]),
    Vertices = get_vertices(Graph),
    io:format(File, "digraph G {~nrankdir=\"TB\"; ordering=out; splines=false;~n", []),
    print_rank_groups(File, Vertices),
    print_timeline(File, Vertices),
    print_pid_edges(File, Vertices),
    print_edges(File, Graph),
    io:format(File, "}~n", []),
    file:close(File).

get_vertices(Graph) ->
    Vertices0 = digraph:vertices(Graph),
    lists:map(fun(V) ->
                      digraph:vertex(Graph, V)
              end, Vertices0).

sort_by_timestamp(Vs) ->
    lists:sort(fun( {_, #event{timestamp = Ts1}}
                  , {_, #event{timestamp = Ts2}}) ->
                       Ts1 < Ts2
               end, Vs).

group_pids(Vertices) ->
    lists:foldl(fun({_, #event{pid = Pid}} = V, Dict) ->
                        dict:append(Pid, V, Dict)
                end, dict:new(), Vertices).

print_timeline(File, Vertices) ->
    Timestamps0 = lists:map(fun({_, #event{timestamp = Timestamp}}) -> Timestamp end,
                            Vertices),
    Timestamps  = lists:sort(Timestamps0),
    Start       = timestamp_to_seconds(hd(Timestamps)) * 1000,
    Timeline = lists:map(
                 fun(Timestamp) ->
                         %% Base = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
                         %% Datetime = calendar:gregorian_seconds_to_datetime(
                         %%              Base + Megas * 1000000 + Secs + Usecs div 1000000),
                         Diff = timestamp_to_seconds(Timestamp) * 1000 - Start,
                         io:format(File, "\"~p\" [shape=none, label=\"~p\"]~n",
                                   [Timestamp, Diff]),
                         lists:flatten(
                           io_lib:format("\"~p\"", [Timestamp]))
                 end, Timestamps),
    io:format(File, "~s [style=dotted,dir=none];~n", [string:join(lists:sort(Timeline), " -> ")]).

timestamp_to_seconds({Mega, Secs, Micro}) ->
    Mega * 1000000 + Secs + Micro / 1000000.0.

print_rank_groups(File, Vertices) ->
    lists:foreach(fun({V, #event{timestamp = Timestamp}}) ->
                          io:format(File, "{rank=same; \"~p\" \"~p\"};~n",
                                    [Timestamp, V])
                  end, Vertices).

print_events(File, Vertices) ->
    lists:foreach(fun({V, #event{ pid = Pid
                                , type = #send_event{ to = To
                                                    , message = Message}}}) ->
                          io:format(File, "\"~p\" [shape=point,label=\"~w ! ~w\"]~n", [V, To, Message]);
                     ({V, #event{ pid = Pid
                                , type=#receive_event{ from = From
                                                     , message = Message}}}) ->
                          io:format(File, "\"~p\" [shape=point,label=\"receive ~w from ~w\"]~n", [V, Message, From])
                  end, Vertices).

print_pid_edges(File, Vertices) ->
    PidGroups = group_pids(Vertices),
    dict:fold(
      fun(Pid, Vs, ok) ->
              Strs = lists:map(
                       fun({V, _}) ->
                               lists:flatten("\"" ++ io_lib:write(V) ++ "\"")
                       end, sort_by_timestamp(Vs)),
              Str  = string:join(Strs, " -> "),
              io:format(File, "{~n node[group=\"~p\"];", [Pid]),
              print_events(File, Vs),
              io:format(File, "\"~p\" [shape=oval]~n", [Pid]),
              io:format(File, "\"~p\" -> ~s [style=dashed,dir=none,weight=10000];~n",
                        [Pid, Str]),
              io:format(File, "}~n", [])
      end, ok, PidGroups).

print_edges(File, Graph) ->
    lists:foreach(fun(E) ->
                          {E, V1, V2, {Serial, Message}} =
                              digraph:edge(Graph, E),
                          {P,C} = Serial,
                          io:format(File, "\"~p\" -> \"~p\" [weight=0, taillabel=\"~w:~w\", label=\"~w\"];~n",
                                    [V1, V2, P, C, Message])
                  end, digraph:edges(Graph)).
