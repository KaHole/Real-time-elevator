-module(state_poller).
-include("../../include/worldstate.hrl").
-export([start/2]).

-define(POLL_RATE, 125).

start(DriverPid, {Elevator, HallCalls}) ->
    register(state_poller, spawn(fun() -> state_server(Elevator, HallCalls) end)),
    spawn(fun() -> poller(DriverPid, length(HallCalls)) end).


state_server(Elevator, HallCalls) ->
    
    receive
        {polled_state_update, {#elevator{floor=Floor, cabCalls=CabCalls}, IncomingHallCalls}} ->

            _CabCalls = [A or B || {A,B} <- lists:zip(Elevator#elevator.cabCalls, CabCalls)],

            _Floor = case Floor of
                between_floors -> Elevator#elevator.floor;
                _ -> Floor
            end,

            _Elevator = Elevator#elevator{floor=_Floor, cabCalls=_CabCalls},

            % Detect changes, send to coordinator if anything to report
            HasIncomingHallCalls = lists:any(fun(E) -> E end, lists:flatten(IncomingHallCalls)),
            if
                (Elevator#elevator.cabCalls =/= _CabCalls)
                or (Elevator#elevator.floor =/= _Floor)
                or HasIncomingHallCalls ->
                    coordinator ! {local_elevator_update, _Elevator, IncomingHallCalls};
                    %io:format("~p~n", ["----- POLLED CHANGES DETECTED -----"]);
                true -> ok
            end,

            % io:format("~p~n", [_Elevator]),
            state_server(_Elevator, HallCalls);

        {driven_state_update, {#elevator{behaviour=Behaviour, direction=Direction, cabCalls=CabCalls}, ActedHallCalls}} ->

            % Detect done cab-calls
            _CabCalls = [if B == done -> false; true -> A end || {A,B} <- lists:zip(Elevator#elevator.cabCalls, CabCalls)],

            _Elevator = Elevator#elevator{behaviour=Behaviour, direction=Direction, cabCalls=_CabCalls},

            % TODO: do we need this?   -  Set done HallCalls to false
            _HallCalls = disarm_hall_calls(HallCalls, ActedHallCalls),

            HasDoneHallCalls = lists:any(fun(E) -> E == done end, lists:flatten(ActedHallCalls)),
            if
                (Elevator#elevator.cabCalls =/= _CabCalls)
                or (Elevator#elevator.behaviour =/= Behaviour)
                or (Elevator#elevator.direction =/= Direction)
                or HasDoneHallCalls ->
                    coordinator ! {local_elevator_update, _Elevator, ActedHallCalls};
                    %io:format("~p~n", ["----- DRIVEN CHANGES DETECTED -----"]);
                true -> ok
            end,

            state_server(_Elevator, _HallCalls);

        {set_hall_calls, _HallCalls} -> state_server(Elevator, _HallCalls);

        {get_state, Sender} -> Sender ! {updated_state, {Elevator, HallCalls}}
    end,

    state_server(Elevator, HallCalls).

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