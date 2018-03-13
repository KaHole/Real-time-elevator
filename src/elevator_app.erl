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


    WorldState = make_worldState(?NUM_FLOORS),
    
    % elevator_logic:start( make_elevator(?NUM_FLOORS)),
    discover:start(),
    node:start(),
    coordinator:start(WorldState),

    elevator_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% MOVE this??
make_worldState(NumFloors) ->
    
    HallRequests = lists:duplicate(NumFloors, {#hallRequest{}, #hallRequest{}}),
    
    LocalElevator = {node(), make_elevator(NumFloors)},

    #worldState{hallRequests=HallRequests, elevators=[LocalElevator]}.
    
make_elevator(NumFloors) ->

    CabRequests = lists:duplicate(NumFloors, false),
    #elevator{cabRequests=CabRequests}.
    

