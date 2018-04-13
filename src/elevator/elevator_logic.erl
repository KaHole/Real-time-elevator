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
    timer:sleep(10),
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    init(Pid, #elevator{floor=Floor});

init(Pid, _) -> 
    elevator_interface:set_motor_direction(Pid, stop).


elevator_controller(Pid) -> 
    % io:fwrite("ele_ctrl~n"),
    % TODO: needs testing of different rates, CAN PROBABLY GO LOWER ??
    %timer:sleep(250),
    timer:sleep(125),
    state_poller ! {get_state, self()},

    receive
        {updated_state, {State, HallCalls}} -> 
            % elevator_interface:set_floor_indicator(Pid, State#elevator.floor),
            
            % Handle hall calls as cab calls temporarily for determining direction of elevator
            CabHallCall = [ Cab or Up or Down || {Cab,[Up,Down]} <- lists:zip(State#elevator.cabCalls, HallCalls)],
            % Figure out which direction to go
            _State = elevator_algorithm(State, CabHallCall),

            % it ensures the elevator stops at a floor.
            % If the hallrequest gets reassigned while elevator is in motion and in between 2 floors
            case _State#elevator.direction of
                stop ->
                    init(Pid, #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)});
                _ -> elevator_interface:set_motor_direction(Pid, _State#elevator.direction)
            end,

            % Check if arrive at a wanted floor
            check_arrival(Pid, _State, CabHallCall, HallCalls)

    end,
    elevator_controller(Pid).

elevator_algorithm(State, CabHallCall) ->
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
                State#elevator{behaviour=moving, direction=up};
            Go_down ->
                State#elevator{behaviour=moving, direction=down};
            true -> 
                State#elevator{
                    behaviour=idle,
                    direction=stop,
                    cabCalls=lists:duplicate(length(State#elevator.cabCalls), false)
                }
        end;
        _ -> State
    end.

check_arrival(_, #elevator{betweenFloors=true}, _, _) ->
    ok;

check_arrival(Pid, State, CabHallCall, HallCalls) ->
    CabStop = lists:nth(
        State#elevator.floor+1,
        State#elevator.cabCalls
    ),

    [HallUp, HallDown] = lists:nth(
        State#elevator.floor+1,
        HallCalls
    ),

    Tmp1 = lists:any(fun(X) -> X end, headnth(State#elevator.floor+1, CabHallCall)),

    HallUpStop = if
        HallUp and (State#elevator.direction == up) -> true;
        HallUp and (State#elevator.direction == down) and (not Tmp1) -> true;
        true -> false
    end,

    Tmp2 = lists:any(fun(X) -> X end, lists:nthtail(State#elevator.floor+1, CabHallCall)),
    HallDownStop = if
        HallDown and (State#elevator.direction == down) -> true;
        HallDown and (State#elevator.direction == up) and (not Tmp2) -> true;
        true -> false
    end, 

    _State = if 
        CabStop -> State#elevator{
            cabCalls=setnth(
                State#elevator.floor+1,
                State#elevator.cabCalls,
                done
            )
        };
        true -> State
    end,

    _HallCalls = if 
        HallUpStop ->
            setnth(State#elevator.floor+1, HallCalls, [done, HallDown]);
        HallDownStop ->
            setnth(State#elevator.floor+1, HallCalls, [HallUp, done]);
        true -> HallCalls
    end,

    if
        CabStop or HallUpStop or HallDownStop -> 
            state_poller ! {driven_state_update, {_State#elevator{behaviour=doorOpen,direction=stop}, _HallCalls}},
            stop_at_floor(Pid, State#elevator.floor);
        true -> state_poller ! {driven_state_update, {_State, _HallCalls}}
    end. 
    % state_poller ! {driven_state_update, {_State, _HallCalls}},

    % if
    %     CabStop or HallUpStop or HallDownStop ->
    %         io:fwrite("Stopping! Opening doors~n"),
    %         stop_at_floor(Pid, State#elevator.floor);
    %     true -> ok
    % end.

stop_at_floor(Pid, Floor) ->
    elevator_interface:set_motor_direction(Pid, stop),
    elevator_interface:set_order_button_light(Pid, cab, Floor, off),
    elevator_interface:set_door_open_light(Pid, on),
    timer:sleep(2000),  % Remain open for 2 sec. Alt. move to case beneath.
    case elevator_interface:get_obstruction_switch_state(Pid) of
        1 -> stop_at_floor(Pid, Floor);
        _ -> ok
    end,
    elevator_interface:set_door_open_light(Pid, off). % Close within 5 seconds?

% https://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

% Gets the first nth-1 elements
headnth(1, _) -> [];
headnth(Nth, [Head|Tail]) -> [Head|headnth(Nth-1, Tail)].
