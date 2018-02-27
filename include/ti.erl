
-module(ti).

-export([start/0]).

-include("worldstate.hrl").


start() ->

    El = #elevator{},
    Tmp = #worldstate{elevators=[El]},

    io:format("~p~n", [Tmp]).
