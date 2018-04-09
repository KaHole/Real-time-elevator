%%%-------------------------------------------------------------------
%% @doc elevator public API
%% @end
%%%-------------------------------------------------------------------

-module(elevator_app).

-behaviour(application).

-include("../include/worldstate.hrl").

%% Constants
-define(NUM_FLOORS, 4).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    % -define(COOKIE, "bananpose_999").
    % erlang:set_cookie(self(), ?COOKIE),

    Elevator = make_elevator(),
    WorldState = make_world_state(Elevator),
    
    io:format("ARGS: ~p~n", [_StartArgs]),

    {_, Port} = application:get_env(port),

    io:format("PORT: ~p~n", [Port]),

    {_, DriverPid} = elevator_interface:start({127,0,0,1}, Port),

    %elevator_logic:start( make_elevator(?NUM_FLOORS), WorldState#worldState.hallRequests),
    discover:start(),
    coordinator:start(WorldState),

    state_poller:start(DriverPid, {Elevator, make_hall_calls()}),

    elevator_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% MOVE to worldstate.hrl ?? .. will have to move includes beneath exports, but thats fine!
make_world_state(Elevator) ->
    HallRequests = lists:duplicate(?NUM_FLOORS, {#hallRequest{}, #hallRequest{}}),
    LocalElevator = {node(), Elevator},
    {[LocalElevator], HallRequests}.
    
make_elevator() ->
    CabRequests = lists:duplicate(?NUM_FLOORS, false),
    #elevator{cabRequests=CabRequests}.

make_hall_calls() ->
    lists:duplicate(?NUM_FLOORS, [false, false]).
