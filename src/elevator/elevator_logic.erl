-module(elevator_logic).
-include("../../include/worldstate.hrl").

-export([start/0]).

start() ->
    {_, Pid} = elevator_interface:start(),
    io:fwrite("~p~n", [Pid]),
    init(Pid, #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}),
    
    % Cab_server_pid = spawn(fun() -> cab_server(
    %         Pid, 
    %         #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}) 
    %     end),
    % register(cab_server, Cab_server_pid),
    % % io:fwrite("~p~n", [Cab_server_pid]),

    % Button_poller_pid = spawn(fun() -> button_poller(
    %         Pid,
    %         lists:duplicate(4, false)) 
    %     end).
    Dummy_state = #elevator{
        behaviour=idle,
        floor=elevator_interface:get_floor_sensor_state(Pid),
        direction=stop,
        cabRequests=lists:duplicate(4, false)
    },
    register(elevator_controller, 
        spawn(fun() -> 
            elevator_controller(
                Pid, 
                Dummy_state
            )
            end
        )
    ).


init(Pid, #elevator{floor=between_floors}) ->
    elevator_interface:set_motor_direction(Pid, up),
    timer:sleep(500),
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    init(Pid, #elevator{floor=Floor});

init(Pid, _) -> 
    elevator_interface:set_motor_direction(Pid, stop),
    ok.

elevator_controller(Pid, State) -> 
    % Checks for pressed cab floor panel buttons
    Polled_panel_state = get_floor_panel_state(Pid, [], length(State#elevator.cabRequests)),
    
    % Union of previous panel state and new polled state
    New_panel_state = [A or B || {A,B} <- lists:zip(State#elevator.cabRequests, Polled_panel_state)],
    
    % Figure out which direction to go
    New_state = elevator_algorithm(Pid, State#elevator{cabRequests=New_panel_state}),
    elevator_interface:set_motor_direction(Pid, New_state#elevator.direction),

    % Check if arrive at a wanted floor
    New_new_state = check_arrival(Pid, New_state),

    elevator_controller(Pid, New_state).

% button_poller(Pid, Floors) ->
%     time:sleep(100),
%     New_floors_state = get_floor_state(Pid, Floors, length(Floors))
%     cab_server ! {cab_call_list, New_floors_state},
%     button_poller(Pid, Floors).

get_floor_panel_state(Pid, Floor_list, 0) ->
    Floor_state = elevator_interface:get_order_button_state(Pid, 0, cab),
    [A == 1 || A <- lists:append([Floor_state], Floor_list)];

get_floor_panel_state(Pid, Floor_list, Floor_number) -> 
    Floor_state = elevator_interface:get_order_button_state(Pid, Floor_number, cab),
    get_floor_panel_state(Pid, lists:append([Floor_state], Floor_list), Floor_number-1).

elevator_algorithm(Pid, State) ->
    {Cab_request_down, Cab_request_up} = lists:split(State#elevator.floor, State#elevator.cabRequests),

    Go_up = lists:any(fun(X) -> X end, [lists:nth(State#.elevator.floor)] ++ Cab_request_up),
    Go_down = lists:any(fun(X) -> X end, Cab_request_down),
    Continue = case State#elevator.direction of
        up -> Go_up;
        down -> Go_down;
        stop -> false
    end,

    case Continue of 
        false -> 
        % New_state = if 
        if
            Go_up ->
                State#elevator{
                    direction=up
                }
            ;
            Go_down ->
                State#elevator{
                    direction=down
                }
            ;
            true -> 
                State#elevator{
                    behaviour=idle,
                    direction=stop,
                    cabRequests=lists:duplicate(length(State#elevator.cabRequests), false)
                }
        end,
    end.
    % New_state.

check_arrival(Pid, New_state) ->
    ok.


% cab_server(Pid, State) ->
%     receive
%         {cab_call, Floor} -> cab_server(Pid, cab_call(Pid, State, Floor));
%         {cab_call_list, Floors} ->
%         % {hall_call, List} -> ;
%         _ -> io:format("njet~n"),
%                 cab_server(Pid, State)
%     end.

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
    


    