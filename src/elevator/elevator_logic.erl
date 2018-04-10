-module(elevator_logic).
-include("../../include/worldstate.hrl").

-export([start/1]).

start(Pid) ->
    io:fwrite("~p~n", [Pid]),
    init(Pid, #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}),

    register(elevator_controller, 
        spawn(fun() -> elevator_controller(Pid) end)
    ).

init(Pid, #elevator{floor=between_floors}) ->
    elevator_interface:set_motor_direction(Pid, up),
    timer:sleep(500),
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    init(Pid, #elevator{floor=Floor});

init(Pid, _) -> 
    elevator_interface:set_motor_direction(Pid, stop).

elevator_controller(Pid) -> 
    % Checks for pressed cab floor panel buttons
    % Polled_panel_state = get_floor_panel_state(Pid, [], length(State#elevator.cabCalls)),
    timer:sleep(250),
    state_poller ! {get_state, self()},
    receive
        {updated_state, {State, HallCalls}} -> 
            % Handle hall calls as cab calls temporarily for determining direction of elevator
            CabHallCall = [ Cab or Up or Down || {Cab,[Up,Down]} <- lists:zip(State#elevator.cabCalls, HallCalls)],
            % Figure out which direction to go
            _State = elevator_algorithm(State, CabHallCall),

            % Check if arrive at a wanted floor
            {NewState, _HallCalls} = check_arrival(Pid, _State, CabHallCall, HallCalls),

            elevator_interface:set_motor_direction(Pid, NewState#elevator.direction),
            elevator_interface:set_floor_indicator(Pid, NewState#elevator.floor),
            state_poller ! {driven_state_update, {NewState, _HallCalls}}
    end,
    elevator_controller(Pid).

elevator_algorithm(State, CabHallCall) ->
    % {Cab_request_down, Cab_request_up} = lists:split(State#elevator.floor+1, State#elevator.cabCalls),

    % Go_up = lists:any(fun(X) -> X end, 
    %     [lists:nth(State#elevator.floor+1, State#elevator.cabCalls)] ++ Cab_request_up
    % ),

    {Cab_request_down, Cab_request_up} = lists:split(State#elevator.floor+1, CabHallCall),

    Go_up = lists:any(fun(X) -> X end, 
        [lists:nth(State#elevator.floor+1, CabHallCall)] ++ Cab_request_up
    ),

    Go_down = lists:any(fun(X) -> X end, Cab_request_down),
    Continue = case State#elevator.direction of
        up -> Go_up;
        down -> Go_down;
        stop -> false
    end,

    case Continue of 
        false -> 
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
                    cabCalls=lists:duplicate(length(State#elevator.cabCalls), false)
                }
        end;
        _ -> State
    end.

check_arrival(Pid, State, CabHallCall, HallCalls) ->
    % StopAtFloor = lists:nth(
    %     State#elevator.floor+1,
    %     State#elevator.cabCalls
    % ),

    StopAtFloor = lists:nth(
        State#elevator.floor+1,
        CabHallCall
    ),

    % If we stop return new state, else return old
    case StopAtFloor of
        true -> stop_at_floor(Pid, State, HallCalls);
        _ -> {State, HallCalls}
    end.

stop_at_floor(Pid, State, HallCalls) ->
    elevator_interface:set_motor_direction(Pid, stop),
    elevator_interface:set_order_button_light(Pid, cab, State#elevator.floor, off),
    elevator_interface:set_door_open_light(Pid, on),
    timer:sleep(2000),  % Remain open for 2 sec. Alt. move to case beneath.
    case elevator_interface:get_obstruction_switch_state(Pid) of
        1 -> stop_at_floor(Pid, State, HallCalls);
        _ -> ok
    end,
    elevator_interface:set_door_open_light(Pid, off), % Close within 5 seconds?
    
    _State = State#elevator{
        cabCalls=setnth(
            State#elevator.floor+1,
            State#elevator.cabCalls,
            done
        )
    },
    [Up, Down] = lists:nth(
        State#elevator.floor+1,
        HallCalls
    ),
    _HallCalls = if 
        Up and State#elevator.direction == up -> setnth(
            State#elevator.floor+1,
            HallCalls,
            [done, Down]
        );
        Down and State#elevator.direction == down -> setnth(
            State#elevator.floor+1,
            HallCalls,
            [Up, done]
        );
        true -> HallCalls
    end,
    {_State, _HallCalls}.

% https://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].
