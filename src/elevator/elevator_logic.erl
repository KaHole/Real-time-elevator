-module(elevator_logic).
-include("../../include/worldstate.hrl").

-export([start/0]).

start() ->
    {_, Pid} = elevator_interface:start(),
    io:fwrite("~p~n", [Pid]),
    init(Pid, #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}),
    
    Cab_server_pid = spawn(fun() -> cab_server(
            Pid, 
            #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}) 
        end),
    io:fwrite("~p~n", [Cab_server_pid]).

init(Pid, #elevator{floor=between_floors}) ->
    elevator_interface:set_motor_direction(Pid, up),
    timer:sleep(500),
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    init(Pid, #elevator{floor=Floor});

init(Pid, _) -> 
    elevator_interface:set_motor_direction(Pid, stop),
    ok.

cab_server(Pid, State) ->
    receive
        {_, Floor} -> cab_server(Pid, cab_call(Pid, State, Floor));
        _ -> io:format("njet~n"),
                cab_server(Pid, State)
    end.

cab_call(Pid, State, Floor) ->
    elevator_interface:set_door_open_light(Pid, off),
    elevator_interface:set_order_button_light(Pid, cab, Floor, on),
    elevator_interface:set_floor_indicator(Pid, State#elevator.floor),
    
    ToFloor = State#elevator.floor - Floor,
    Direction = if 
        ToFloor > 0 -> down;
        ToFloor < 0 -> up;
        true -> stop
    end,
    move(Pid, 
        State#elevator{behaviour=idle, direction=Direction},
        Floor).

move(Pid, #elevator{direction=stop} = State, NewFloor) ->
    elevator_interface:set_motor_direction(Pid, State#elevator.direction),
    elevator_interface:set_door_open_light(Pid, on),
    elevator_interface:set_order_button_light(Pid, cab, State#elevator.floor, off),
    elevator_interface:set_floor_indicator(Pid, State#elevator.floor),
    State;

move(Pid, #elevator{behaviour=moving} = State, NewFloor) ->
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    timer:sleep(100),
    elevator_interface:set_floor_indicator(Pid, State#elevator.floor),
    if 
        Floor == NewFloor -> move(Pid, 
                State#elevator{behaviour=idle, direction=stop, floor=Floor}, NewFloor);
        Floor /= between_floors -> move(Pid, State#elevator{floor=Floor}, NewFloor);
        true -> move(Pid, State, NewFloor)
    end;

move(Pid, State, NewFloor) ->
    elevator_interface:set_motor_direction(Pid, State#elevator.direction),
    timer:sleep(500),
    move(Pid, State#elevator{behaviour=moving}, NewFloor).
    


    