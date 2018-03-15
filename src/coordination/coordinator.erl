-module(coordinator).

-include("../../include/worldstate.hrl").

-export([start/1]).

start(WorldState) ->
    register(coordinator, spawn(fun() -> observe(WorldState) end)),
    ok.

% BIBELEN:
% WHEN CALLING HALL_REQUEST_ASSIGNER, FILTER ON nodes() TO ONLY SEND THE ACTIVE ONES!!


% LOGIC

% observe() ->
%     local ->
%         broadcast().

%     external ->
%         consense().

% consense() ->
%     hall_request_assign(),
%     send_to_elevator_logic(),
%     broadcast().

% broadcast() ->
%     send_to_all_nodes() % obviously
%     observe().

% /logic

observe(WorldState) ->

    receive

        {local_elevator_update, Elevator, HallButtons} ->
            io:format("local elevator~n"),

            _Elevators = update_elevator_list(WorldState, node(), Elevator),

            io:format("updated elevators: ~p~n", [_Elevators]),

            % TODO: Create hall-requests that do not already exist
            
            % TODO: broadcast new state!!

            _HallRequests = add_hall_requests(WorldState#worldState.hallRequests);

        {elevator_update, Id, Elevator, HallRequests} ->
            io:format("foreign elevator~n"),

            _Elevators = update_elevator_list(WorldState, node(), Elevator),

            % TODO: Integrate HallRequests in some clever way

            _HallRequests = WorldState#worldState.hallRequests

    end,

    % consense if necessary
    %   -> and if changes, we have to broadcasr again
    %       -> make sure this doesnt become a clusterfuck
    %           the main loop might need some restructuring
    %               USE LOGIC outline above to figure it out
    %
    % assign hall_requests (always), should always update

    observe(WorldState#worldState{elevators=_Elevators, hallRequests=_HallRequests}).


update_elevator_list(WorldState, Id, Elevator) ->
    lists:keyreplace(Id, 1, WorldState#worldState.elevators, {Id, Elevator}).


add_hall_requests(WorldState, HallButtons) ->
    % create hall_requests based on HallButtons
    %   ignore if there is an existing hall_request with certain states i guess?
    .

update_hall_requests(WorldState, HallRequests) ->
    % merge hall requests from another elevators message
    .


broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].

