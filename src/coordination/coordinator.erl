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
    coordinator ! {elevator_update, #elevator{id=node()}},
    ok.


observe(WorldState) ->

    receive

        {elevator_update, Elevator} when Elevator#elevator.id =:= node() ->
            io:format("local elevator~n");
            % 1. update model
            % 2. then send

        {elevator_update, Elevator} -> io:format("foreign elevator~n")

    end,


    observe(WorldState).


% ----
% % To check if alive when sending message
% % process can be restarted at the undefined match

% send(undefined) -> ok;

% send(Pid, Msg) ->
%     Pid ! Msg.

