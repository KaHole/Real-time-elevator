-module(node).
-export([start/0]).

% Denne kan flyttes ut til øverste nivå.. eller i app configen
% -define(COOKIE, "bananpose_999").
% erlang:set_cookie(self(), ?COOKIE),

start() ->
    register(node, spawn(fun() -> loop() end)),

loop() ->
    receive
        {broadcast, Msg} ->
            io:format("broadcasting: ~p~n", [Msg]),
            [{node, N} ! {message, Msg} || N <- nodes()];

        {message, Msg} ->
            % TODO: Videresend til coordinator basically
            io:format("received: ~p~n", [Msg])
    end,
    loop().

% pattern for responding to messages immeadietly
%
% receive
%   {From, Msg} ->
%       From ! response
%
% State machine
%
% use pattern matching to determine state and to define legal actions on that state, great!
%
% machine({0, State}) ->
%   ok.
%
% machine({1, State}) ->
%   ok.
%
% machine({2, State}) ->
%   ok.
