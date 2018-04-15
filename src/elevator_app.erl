-module(elevator_app).
-behaviour(application).

-include("../include/worldstate.hrl").
-export([start/2, stop/1]).

-define(NUM_FLOORS, 4).
-define(TICKTIME, 15).

start(_StartType, _StartArgs) ->

    % TODO:
    % Viktig info for release:
    % relx gir appen short-name (node navn) by default "dev_elevator@datamaskin"
    % Den setter også en cookie by default; samme verdi "dev_elevator"
    % Disse kan selvsagt settes i relx config / vim.config som pekes på, men er gode defaults egentlig.

    erlang:set_cookie(node(), "elevator_bananpose"),

    net_kernel:stop(),

    {_, [{addr, Ip}|_]} = inet:ifget("eno1", [addr]),

    IpString = inet_parse:ntoa(Ip),
    Name = "elevator@" ++ IpString,

    net_kernel:start([list_to_atom(Name), longnames, ?TICKTIME]),

    % Set tick rate for erlang detecting down nodes
    %net_kernel:set_net_ticktime(?TICKTIME),

    Elevator = make_elevator(),
    WorldState = make_world_state(Elevator),

    % Release skal helst starte alt med en binary om mulig, uten config

    %TODO: Fjerne dette før release
    %{_, Port} = application:get_env(port),

    %{_, DriverPid} = elevator_interface:start({127,0,0,1}, Port),
    {_, DriverPid} = elevator_interface:start(),

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

%% MOVE to worldstate.hrl ?? .. will have to move includes beneath exports, but thats fine!
make_world_state(Elevator) ->
    HallRequests = lists:duplicate(?NUM_FLOORS, {#hallRequest{}, #hallRequest{}}),
    LocalElevator = {node(), Elevator},
    {[LocalElevator], HallRequests}.
    
make_elevator() ->
    CabCalls = lists:duplicate(?NUM_FLOORS, false),
    #elevator{cabCalls=CabCalls}.

make_hall_calls() -> lists:duplicate(?NUM_FLOORS, [false, false]).