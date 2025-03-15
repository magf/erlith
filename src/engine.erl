-module(engine).
-export([start/1, spawn_entity/2, send_message/2, subscribe_monitor/1]).

start(ConfigFile) ->
    mnesia:start(),
    mnesia:create_table(entities, [{attributes, [id, props]}, {type, set}]),
    {ok, Config} = load_config(ConfigFile),
    WorldPid = spawn(fun() -> world_loop(Config, []) end),
    {ok, WorldPid}.

load_config(File) ->
    case file:read_file(File) of
        {ok, Json} ->
            Config = jsx:decode(Json, [return_maps]),
            {ok, Config};
        {error, Reason} ->
            error({config_load_failed, Reason})
    end.

subscribe_monitor(WorldPid) ->
    WorldPid ! {subscribe, self()},
    ok.

world_loop(Config, Monitors) ->
    receive
        Msg ->
            io:format("World received: ~p, Monitors: ~p~n", [Msg, Monitors]),
            case Msg of
                {subscribe, MonitorPid} ->
                    world_loop(Config, [MonitorPid | Monitors]);
                {spawn, Id, EntityConfig} ->
                    {ok, Pid} = spawn_entity(Id, EntityConfig),
                    notify_monitors(Monitors, {spawned, Id, EntityConfig}),
                    world_loop(Config, Monitors);
                {message, FromId, ToId, Msg} ->
                    send_message(ToId, Msg),
                    notify_monitors(Monitors, {message_sent, FromId, ToId, Msg}),
                    world_loop(Config, Monitors);
                {spawn_random, TemplateName} ->
                    RandomConfig = generate_random_entity(maps:get(<<"random_templates">>, Config), TemplateName),
                    {ok, Pid} = spawn_entity(random_id(), RandomConfig),
                    notify_monitors(Monitors, {spawned_random, TemplateName, RandomConfig}),
                    world_loop(Config, Monitors)
            end
    end.

spawn_entity(Id, Config) ->
    Props = maps:get(<<"props">>, Config, #{}),
    Behavior = binary_to_atom(maps:get(<<"behavior">>, Config, <<"generic_behavior">>), utf8),
    io:format("Spawning ~p with behavior ~p~n", [Id, Behavior]),
    Pid = spawn(Behavior, loop, [Id, Props]),
    register(list_to_atom(binary_to_list(Id)), Pid),
    mnesia:write({entities, Id, Props}),
    {ok, Pid}.

send_message(Id, Msg) ->
    case mnesia:read({entities, Id}) of
        [{entities, Id, Props}] ->
            Pid = whereis(list_to_atom(binary_to_list(Id))),
            Pid ! {message, Msg};
        [] ->
            {error, not_found}
    end.

notify_monitors(Monitors, Event) ->
    lists:foreach(fun(Pid) -> Pid ! {event, Event} end, Monitors).

generate_random_entity(Templates, TemplateName) ->
    Template = maps:get(TemplateName, Templates),
    Props = maps:map(
        fun(Key, #{<<"type">> := <<"int">>, <<"range">> := [Min, Max]}) ->
            rand:uniform(Max - Min) + Min;
           (Key, #{<<"type">> := <<"float">>, <<"range">> := [Min, Max]}) ->
            Min + rand:uniform() * (Max - Min)
        end, maps:get(<<"props">>, Template)),
    #{<<"props">> => Props, <<"behavior">> => maps:get(<<"behavior">>, Template, <<"generic_behavior">>)}.

random_id() ->
    list_to_binary("entity_" ++ integer_to_list(rand:uniform(1000000))).
