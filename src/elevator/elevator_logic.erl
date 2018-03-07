-module(elevator_logic).
-include("../../include/worldstate.hrl").

-export([start/0]).

start() ->
    {_, Pid} = elevator_interface:start(),
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    io:fwrite("~p~n", [Pid]),
    State = #elevator{floor=Floor},
    init(Pid, State).

init(Pid, #elevator{floor=between_floors}) ->
    elevator_interface:set_motor_direction(Pid, up),
    timer:sleep(500),
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    init(Pid, #elevator{floor=Floor});

init(Pid, _) -> 
    elevator_interface:set_motor_direction(Pid, stop),
    ok.


