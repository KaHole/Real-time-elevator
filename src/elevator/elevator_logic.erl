-module(elevator_logic).

-export([start/0)]).

start() ->
    {_, Pid} = elevator_interface:start().