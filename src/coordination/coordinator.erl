-module(coordinator).

-include("../../include/worldstate.hrl").

-export([start/0]).

% Start procedures overalt bør nå slutten og spawne ting som skal fortsette
% tror det er det beste, så kommunisere med messages
% så kan ting restartes enklere og

start() ->
    % node:start(),
    % elevator_logic:start(),

    WorldState = #worldState{},

    register(coordinator, spawn(fun() -> observe(WorldState) end)),

    % test
    % coordinator ! {elevator_update, #elevator{id=node()}},
    ok.

observe(WorldState) ->

    receive

        {elevator_update, Elevator} ->
            io:format("local elevator~n"),
            % 1. update model
            % 2. then send

            lists:keyreplace(node(), 1, WorldState#worldState.elevators, {node(), Elevator});


        % {elevator_update, {HallRequests, Elevator}} -> io:format("foreign elevator~n")
        #stateMsg{} = StateMsg ->
            io:format("foreign elevator~n"),
            % lists:keyreplace(StateMsg#stateMsg.elevator , 1, WorldState#worldState.elevators, {node(), Elevator})
        % ForeignState#localState{} -> io:format("foreign elevator~n")

    end,


    observe(WorldState).


% BIBELEN:
% WHEN CALLING HALL_REQUEST_ASSIGNER, FILTER ON nodes() TO ONLY SEND THE ACTIVE ONES!!
