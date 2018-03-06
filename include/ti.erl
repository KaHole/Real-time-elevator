
-module(ti).

-export([start/0]).

-include("worldstate.hrl").


start() ->

    State = #elevator{},

    Tmp = #worldstate{elevators=[State]},

    io:format("~p~n", [Tmp]).
