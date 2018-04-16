-module(elevator_app).
-behaviour(application).

-include("../include/worldstate.hrl").
-export([start/2, stop/1]).

-define(NUM_FLOORS, 4).
-define(TICKTIME, 15000).

start(_StartType, _StartArgs) ->

    {_, Interface} = inet:ifget("eno1", [addr]),

    case Interface of
        [{addr, Ip}|_] ->
            Name = list_to_atom("elevator@" ++ inet_parse:ntoa(Ip)),
            net_kernel:stop(),
            net_kernel:start([Name, longnames, ?TICKTIME]);
        _ -> net_kernel:set_net_ticktime(?TICKTIME)
    end,

    erlang:set_cookie(node(), 'bananpose'),

    Elevator = make_elevator(),
    WorldState = make_world_state(Elevator),

    % Used for local testing with multiple elevators
    % {_, Port} = application:get_env(port),
    % {_, DriverPid} = elevator_interface:start({127,0,0,1}, Port),

    {_, DriverPid} = elevator_interface:start(),
    register(driver_pid, DriverPid),

    watchdog:start(lists:duplicate(?NUM_FLOORS, [nothing,nothing])),
    discover:start(),
    coordinator:start(WorldState),
    state_poller:start(DriverPid, {Elevator, make_hall_calls()}),
    elevator_logic:start(DriverPid),

    elevator_sup:start_link().

stop(_State) ->
    ok.

make_world_state(Elevator) ->
    HallRequests = lists:duplicate(?NUM_FLOORS, {#hallRequest{}, #hallRequest{}}),
    LocalElevator = {node(), Elevator},
    {[LocalElevator], HallRequests}.
    
make_elevator() ->
    CabCalls = lists:duplicate(?NUM_FLOORS, false),
    #elevator{cabCalls=CabCalls}.

make_hall_calls() -> lists:duplicate(?NUM_FLOORS, [false, false]).