-module(node).
% -export([start/0]).

% Denne kan flyttes ut til øverste nivå.. eller i app configen
% -define(COOKIE, "bananpose_999").
% erlang:set_cookie(self(), ?COOKIE),

% TRENGER IKKE DENNE, BARE ADDRESSER COORDINATOR DIREKTE??? !!

 % start() ->
 %     ok.
     % register(node, spawn(fun() -> loop() end)).

% loop() ->
%     receive
%         {broadcast, Msg} ->
%             io:format("broadcasting: ~p~n", [Msg]),
%             [{node, N} ! {message, Msg} || N <- nodes()];

%         {message, Msg} ->
%             % TODO: Videresend til coordinator basically
%             io:format("received: ~p~n", [Msg])
%     end,
%     loop().

% broadcast_state(Elevator, HallRequests) ->
%     [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].

% pattern for responding to messages immeadietly
%
% receive
%   {From, Msg} ->
%       From ! response
%
