-module(state_poller).
-include("../../include/worldstate.hrl").
-export([start/2]).

-define(POLL_RATE, 180).

start(DriverPid, {Elevator, HallCalls}) ->
    register(state_poller, spawn(fun() -> state_server(DriverPid, Elevator, HallCalls) end)),
    spawn(fun() -> poller(DriverPid, length(HallCalls)) end).


state_server(DriverPid, Elevator, HallCalls) ->
    
    receive
        {get_state, Sender} -> Sender ! {updated_state, {Elevator, HallCalls}}
        after 0 -> receive
            {polled_state_update, {#elevator{floor=Floor, cabCalls=CabCalls}, IncomingHallCalls}} ->

                _CabCalls = [A or B || {A,B} <- lists:zip(Elevator#elevator.cabCalls, CabCalls)],
                {_Floor, _BetweenFloors} = case Floor of
                    between_floors -> {Elevator#elevator.floor, true};
                    _ -> {Floor, false}
                end,

                _Elevator = Elevator#elevator{floor=_Floor, cabCalls=_CabCalls, betweenFloors=_BetweenFloors},

                % Detect changes, send to coordinator if anything to report
                HasIncomingHallCalls = lists:any(fun(E) -> E end, lists:flatten(IncomingHallCalls)),
                if
                    (Elevator#elevator.cabCalls =/= _CabCalls)
                    or (Elevator#elevator.floor =/= _Floor)
                    or HasIncomingHallCalls ->

                        elevator_interface:set_floor_indicator(DriverPid, _Floor),
                        set_cab_button_lights(DriverPid, _CabCalls),

                        coordinator ! {local_elevator_update, _Elevator#elevator{direction=get_valid_direction(_Elevator)}, IncomingHallCalls};

                    true -> ok
                end,

                state_server(DriverPid, _Elevator, HallCalls);

            {driven_state_update, {#elevator{behaviour=Behaviour, direction=Direction, cabCalls=CabCalls}, ActedHallCalls}} ->

                % Detect done cab-calls
                _CabCalls = [if B == done -> false; true -> A end || {A,B} <- lists:zip(Elevator#elevator.cabCalls, CabCalls)],

                set_cab_button_lights(DriverPid, _CabCalls),

                _Elevator = Elevator#elevator{behaviour=Behaviour, direction=Direction, cabCalls=_CabCalls},

                % Set done HallCalls to false locally aswell
                _HallCalls = disarm_hall_calls(HallCalls, ActedHallCalls),

                HasDoneHallCalls = lists:any(fun(E) -> E == done end, lists:flatten(ActedHallCalls)),

                if
                    (Elevator#elevator.cabCalls =/= _CabCalls)
                    or (Elevator#elevator.behaviour =/= Behaviour)
                    or (Elevator#elevator.direction =/= Direction)
                    or HasDoneHallCalls ->
                        coordinator ! {local_elevator_update, _Elevator#elevator{direction=get_valid_direction(_Elevator)}, ActedHallCalls};

                    true -> ok
                end,

                state_server(DriverPid, _Elevator, _HallCalls);

            {set_hall_calls, _HallCalls} -> state_server(DriverPid, Elevator, _HallCalls);

            {get_state, Sender} -> Sender ! {updated_state, {Elevator, HallCalls}};

            {set_hall_order_button_lights, HallRequests} -> set_hall_button_lights(DriverPid, HallRequests)
        end
    end,

    state_server(DriverPid, Elevator, HallCalls).

disarm_hall_calls([], []) -> [];
disarm_hall_calls([[HallUp, HallDown] | Tail], [[ActedHallUp, ActedHallDown] | ActedTail]) ->
    _HallUp = case ActedHallUp of
        done -> false;
        _ -> HallUp
    end,
    _HallDown = case ActedHallDown of
        done -> false;
        _ -> HallDown
    end,
    [[_HallUp, _HallDown] | disarm_hall_calls(Tail, ActedTail)].

get_valid_direction(#elevator{direction=down, floor=0}) ->
    stop;
get_valid_direction(#elevator{direction=up, floor=Floor, cabCalls=CabCalls}) ->
    if 
        Floor =:= length(CabCalls)-1 -> stop;
        true -> up
    end;
get_valid_direction(Elevator) ->
    Elevator#elevator.direction.

set_hall_button_lights(DriverPid, HallRequests) ->
    set_hall_button_lights_internal(DriverPid, HallRequests, 0).

set_hall_button_lights_internal(_, [], _) -> ok;

set_hall_button_lights_internal(DriverPid, [{#hallRequest{state=HallUp}, #hallRequest{state=HallDown}}| Tail], N) ->
    UpOn = case HallUp of
        accepted -> on;
        _ -> off
    end,
    DownOn = case HallDown of
        accepted -> on;
        _ -> off
    end,
    elevator_interface:set_order_button_light(DriverPid, hall_up, N, UpOn),
    elevator_interface:set_order_button_light(DriverPid, hall_down, N, DownOn),
    set_hall_button_lights_internal(DriverPid, Tail, N+1).

set_cab_button_lights(Pid, CabCalls) ->
        set_cab_button_lights_internal(Pid, CabCalls, 0).
        
set_cab_button_lights_internal(_, [], _) -> ok;

set_cab_button_lights_internal(Pid, [CabCall | Tail], N) ->
    LightOn = if
        CabCall -> on;
        true -> off
    end,
    elevator_interface:set_order_button_light(Pid, cab, N, LightOn),
    set_cab_button_lights_internal(Pid, Tail, N+1).

poller(DriverPid, NumFloors) ->

    Elevator = poll_elevator(DriverPid, NumFloors),
    HallCalls = get_hall_calls(DriverPid, NumFloors-1),

    state_poller ! {polled_state_update, {Elevator, HallCalls}},

    timer:sleep(?POLL_RATE),
    poller(DriverPid, NumFloors).

poll_elevator(DriverPid, NumFloors) ->

    Floor = elevator_interface:get_floor_sensor_state(DriverPid),
    CabCalls = get_cab_calls(DriverPid, NumFloors-1),

    #elevator{floor=Floor, cabCalls=CabCalls}.

get_cab_calls(Pid, 0) ->
    [elevator_interface:get_order_button_state(Pid, 0, cab) =:= 1];

get_cab_calls(Pid, Floor) ->
    CabCall = elevator_interface:get_order_button_state(Pid, Floor, cab) =:= 1,
    get_cab_calls(Pid, Floor-1) ++ [CabCall].

get_hall_calls(Pid, 0) ->
    HallUp = elevator_interface:get_order_button_state(Pid, 0, hall_up) =:= 1,
    HallDown = elevator_interface:get_order_button_state(Pid, 0, hall_down) =:= 1,
    [[HallUp, HallDown]];

get_hall_calls(Pid, Floor) ->
    HallUp = elevator_interface:get_order_button_state(Pid, Floor, hall_up) =:= 1,
    HallDown = elevator_interface:get_order_button_state(Pid, Floor, hall_down) =:= 1,
    get_hall_calls(Pid, Floor-1) ++ [[HallUp, HallDown]].