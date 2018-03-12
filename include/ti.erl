
-module(ti).

-export([start/0]).

-include("worldstate.hrl").


start() ->

    State = #elevator{},

    Tmp = #worldState{elevators=[State]},

    io:format("~p~n", [Tmp]),
    go(State).


go(E#elevator{}) ->
    io:format("short syntax~n"),
    E;

go(E) when is_record(E, elevator) ->
    E.
