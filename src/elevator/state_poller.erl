-module(state_poller).

-include("../../include/worldstate.hrl").

-export([start/2]).

-define(POLL_RATE, 250).

start(DriverPid, {Elevator, HallCalls}) ->
    register(state_poller, spawn(fun() -> state_server(Elevator, HallCalls) end)),
    spawn(fun() -> poller(DriverPid, length(HallCalls)) end).


state_server(Elevator, HallCalls) ->
    
    receive
        {polled_state_update, {#elevator{floor=Floor, cabRequests=CabRequests}, IncomingHallCalls}} ->

            _CabRequests = [A or B || {A,B} <- lists:zip(Elevator#elevator.cabRequests, CabRequests)],
            _Elevator = Elevator#elevator{floor=Floor, cabRequests=_CabRequests},

            % Detect changes, send to coordinator if anything to report
            HasIncomingHallCalls = lists:any(fun(E) -> E end, lists:flatten(IncomingHallCalls)),
            if
                (Elevator#elevator.cabRequests =/= _CabRequests)
                or (Elevator#elevator.floor =/= Floor)
                or HasIncomingHallCalls ->
                    coordinator ! {local_elevator_update, _Elevator, IncomingHallCalls},
                    io:format("~p~n", ["----- POLLED CHANGES DETECTED -----"]);
                true -> ok
            end,

            io:format("~p~n", [_Elevator]),

            state_server(_Elevator, HallCalls);

        {driven_state_update, {#elevator{behaviour=Behaviour, direction=Direction, cabRequests=CabRequests}, ActedHallCalls}} ->

            % Detect done cab-calls
            _CabRequests = [if B == done -> false; true -> A end || {A,B} <- lists:zip(Elevator#elevator.cabRequests, CabRequests)],

            _Elevator = Elevator#elevator{behaviour=Behaviour, direction=Direction, cabRequests=_CabRequests},

            HasDoneHallCalls = lists:any(fun(E) -> E == done end, lists:flatten(ActedHallCalls)),
            if
                (Elevator#elevator.cabRequests =/= _CabRequests)
                or (Elevator#elevator.behaviour =/= Behaviour)
                or (Elevator#elevator.direction =/= Direction)
                or HasDoneHallCalls ->
                    coordinator ! {local_elevator_update, _Elevator, ActedHallCalls},
                    io:format("~p~n", ["----- DRIVEN CHANGES DETECTED -----"]);
                true -> ok
            end,

            state_server(_Elevator, HallCalls);

        {set_hall_calls, _HallCalls} ->
            % JA, det går fint, dette er motsatt retning i pipe-linen uansett
            state_server(Elevator, _HallCalls);

        {get_state, Sender} -> Sender ! {Elevator, HallCalls}
    end,

    state_server(Elevator, HallCalls).


poller(DriverPid, NumFloors) ->

    Elevator = poll_elevator(DriverPid, NumFloors),
    HallCalls = get_hall_calls(DriverPid, NumFloors-1),

    state_poller ! {polled_state_update, {Elevator, HallCalls}},

    timer:sleep(?POLL_RATE),
    poller(DriverPid, NumFloors).

poll_elevator(DriverPid, NumFloors) ->

    Floor = elevator_interface:get_floor_sensor_state(DriverPid),
    CabCalls = get_cab_calls(DriverPid, NumFloors-1),

    #elevator{floor=Floor, cabRequests=CabCalls}.

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
