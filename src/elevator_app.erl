-module(elevator_app).
-behaviour(application).

-include("../include/worldstate.hrl").
-export([start/2, stop/1]).

-define(NUM_FLOORS, 4).
-define(TICKTIME, 15).

start(_StartType, _StartArgs) ->

    % TODO:
    % Viktig info for release:
    % må skyte inn riktig node navn
    % Men net_kernel:stop er litt insane måte å gjøre det på

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

    %TODO: Fjerne dette før release
    {_, Port} = application:get_env(port),

    {_, DriverPid} = elevator_interface:start({127,0,0,1}, Port),
    register(driver_pid, DriverPid),
    %{_, DriverPid} = elevator_interface:start(),

    watchdog:start(lists:duplicate(?NUM_FLOORS, [nothing,nothing])),
    discover:start(),
    % Test for mac:
    % {connect, 'one@Kristians-MacBook-Pro-2'} ! ping,
    % {connect, 'two@Kristians-MacBook-Pro-2'} ! ping,
    % {connect, 'three@Kristians-MacBook-Pro-2'} ! ping,
    % {connect, 'four@Kristians-MacBook-Pro-2'} ! ping,
    coordinator:start(WorldState),
    state_poller:start(DriverPid, {Elevator, make_hall_calls()}),
    elevator_logic:start(DriverPid),

    %TODO: fjern og slett den filen? eller skal vi bruke denne til monitoring/fault tolerance?
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