-module(state_poller).

-export([start/1]).

-define(POLL_RATE, 250).

start(DriverPid, {Elevator, HallCalls}) ->
    register(state_poller, spawn(fun() -> server(Elevator, HallCalls) end)).
    spawn(fun() -> poller(DriverPid) end.


server(Elevator, HallCalls) ->
    
    % SEND TO COORDINATOR in relevant cases
    %  Might need to detect changes!
    %   -> either here or in coordinator
    %       -> so that redundant traffic isnt stuffing the network
    %
    %
    % Pass på å sette en "program-rate" fort vekk nødvendig med en sleep i elevator_logic
    %           -> elevator_logic dikterer "program-raten"

    receive
        {polled_state_update, {#elevator{floor=Floor, cabRequests=CabRequests}, IncomingHallCalls}} ->

            % set floor
            % merge cab-calls

            _CabRequests = 

            UpdatedElevator = Elevator#elevator{floor=Floor, cabRequests=_CabRequests},

            % HallCalls skal ikke oppdateres her, bare incoming skal sendes inn til coordinator

            % f.eks
            coordinator ! {local_elevator_update, UpdatedElevator, IncomingHallCalls},

            server(UpdatedElevator, HallCalls);

        {driven_state_update, {_Elevator, ActedHallCalls}} ->
            % Merge cab-calls
            % Merge hallCalls
            % set behaviour, direction
            % dont update floor
            % Pretty sure dont update hall-calls here either!
            server(_Elevator, HallCalls);

        {set_hall_calls, _HallCalls} ->
            
            % Må merge her og, for å ikke overskrive "done" call?
            % ELLER KANSKJE IKKE, "done"-eventet vil bli sendt med en gang uansett!
            % JA, det går fint, dette er motsatt retning i pipe-linen uansett
            server(Elevator, _HallCalls);

        {get_state, Sender} -> Sender ! {Elevator, HallCalls}
    end,

    server(Elevator, HallCalls).


poller(DriverPid) ->

    Elevator = poll_elevator(DriverPid, Elevator),

    state_poller ! {polled_state_update, {Elevator, HallCalls}},

    timer:sleep(?POLL_RATE),
    poller(DriverPid).

poll_elevator(DriverPid) ->

    Floor = elevator_interface:get_floor_sensor_state(DriverPid),

    #elevator{floor=Floor, cabRequests=CabRequests}.
