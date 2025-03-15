-module(monitor).
-export([start/1]).

start(WorldPid) ->
    engine:subscribe_monitor(WorldPid),
    spawn(fun() -> monitor_loop(#{entities => 0, events => 0}) end).

monitor_loop(State) ->
    io:format("\033[2J\033[H"),
    print_stats(State),
    receive
        {event, {spawned, Id, Config}} ->
            NewState = update_state(State, spawned, {Id, Config}),
            monitor_loop(NewState);
        {event, {message_sent, FromId, ToId, Msg}} ->
            NewState = update_state(State, message, {FromId, ToId, Msg}),
            monitor_loop(NewState);
        {event, {spawned_random, TemplateName, Config}} ->
            NewState = update_state(State, spawned_random, {TemplateName, Config}),
            monitor_loop(NewState)
    after 1000 ->
        monitor_loop(State)
    end.

update_state(State, spawned, {Id, Config}) ->
    Entities = maps:get(entities, State),
    io:format("Spawned: ~p with ~p~n", [Id, maps:get(<<"props">>, Config)]),
    State#{entities => Entities + 1};
update_state(State, message, {FromId, ToId, Msg}) ->
    Events = maps:get(events, State),
    io:format("Message: ~p -> ~p: ~p~n", [FromId, ToId, Msg]),
    State#{events => Events + 1};
update_state(State, spawned_random, {TemplateName, Config}) ->
    Entities = maps:get(entities, State),
    io:format("Random spawn from ~p: ~p~n", [TemplateName, maps:get(<<"props">>, Config)]),
    State#{entities => Entities + 1}.

print_stats(State) ->
    io:format("=== Engine Monitor ===~nActive Entities: ~p~nTotal Events: ~p~n=================~n", [maps:get(entities, State), maps:get(events, State)]).
