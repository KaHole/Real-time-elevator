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

    WorldState = make_world_state(?NUM_FLOORS),
    
    % elevator_logic:start( make_elevator(?NUM_FLOORS), WorldState#worldState.hallRequests),
    discover:start(),
    % node:start(),
    coordinator:start(WorldState),

    Ne = make_elevator(?NUM_FLOORS),
    Ue = Ne#elevator{floor=3, direction=up, behaviour=moving},
    coordinator ! {local_elevator_update, Ue, [{true, false}, {false, false}, {false, false}, {false, false} ]},
    % coordinator ! {local_elevator_update, Ue, []},
    % coordinator ! {local_elevator_update, Ue, []},

    elevator_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% MOVE to worldstate.hrl .. will have to move includes beneath exports, but thats fine!
make_world_state(NumFloors) ->
    
    HallRequests = lists:duplicate(NumFloors, {#hallRequest{}, #hallRequest{}}),
    
    LocalElevator = {node(), make_elevator(NumFloors)},

    {[LocalElevator], HallRequests}.
    
make_elevator(NumFloors) ->

    CabRequests = lists:duplicate(NumFloors, false),
    #elevator{cabRequests=CabRequests}.
    

