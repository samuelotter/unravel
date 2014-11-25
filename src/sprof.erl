-module(sprof).

-export([ analyse/1
        %% , profile/1
        , apply/2
        %% , apply/3
        , start/1
        , stop/1
        , start_tracing/1
        , stop_tracing/0
        ]).

%%% * API **********************************************************************

apply(Label, Fun) ->
    start_tracing(Label),
    Self = self(),
    Pid = spawn(fun() ->
                        Result = receive trigger -> Fun() end,
                        Self ! Result
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
    Pid ! {stop, self()},
    receive
        {stopped, Pid} ->
            ok
    after 5000 ->
            timeout
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
            {ok, Events}
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
            %%Events = dump_events(Label, []),
            Events = lists:filter(fun({seq_trace, L, _, _}) ->
                                          L == Label
                                  end, Events0),
            From ! {reply, Events},
            msg_handler([]);
        {stop, From} ->
            From ! {stopped, self()}
    end.

%% dump_events(Label, Acc0) ->
%%     receive
%%         {seq_trace, Label, TraceInfo, TimeStamp} ->
%%             Acc = [{Label, TimeStamp, TraceInfo} | Acc0],
%%             dump_events(Label, Acc);
%%         _ -> Acc0
%%     end.
