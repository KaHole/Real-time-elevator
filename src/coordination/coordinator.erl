-module(coordinator).

-include("../../include/worldstate.hrl").

-export([start/0]).

% Start procedures overalt bør nå slutten og spawne ting som skal fortsette
% tror det er det beste, så kommunisere med messages
% så kan ting restartes enklere og

start() ->
    node:start(),
    elevator_logic:start(),

    WorldState = #worldstate{},

    register(coordinator, spawn(fun() -> observe(WorldState) end)),

    ok.


observe(WorldState) ->

    broadcast ! "yoyoyoy",
    observe(WorldState).



% ----
% % To check if alive when sending message
% % process can be restarted at the undefined match

% send(undefined) -> ok;

% send(Pid, Msg) ->
%     Pid ! Msg.

