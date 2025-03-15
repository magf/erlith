-module(generic_behavior).
-export([loop/2]).

loop(Id, Props) ->
    receive
        {message, {update, NewProps}} ->
            UpdatedProps = maps:merge(Props, NewProps),
            mnesia:write({entities, Id, UpdatedProps}),
            loop(Id, UpdatedProps);
        {message, {get, Key}} ->
            Value = maps:get(Key, Props, undefined),
            io:format("~p: ~p = ~p~n", [Id, Key, Value]),
            loop(Id, Props)
    end.
