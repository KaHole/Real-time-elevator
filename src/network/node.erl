-module(node).
-export([start/0]).

% Denne kan flyttes ut til øverste nivå.. eller i app configen
-define(COOKIE, "bananpose_999").
% erlang:set_cookie(self(), ?COOKIE),

start() ->
    register(networking, spawn(fun() -> listen() end)),
    register(broadcast, spawn(fun() -> broadcast() end)).

broadcast() ->
    receive
        {broadcast, Msg} ->
            io:format("broadcasting: ~p~n", [Msg]),
            [{networking, N} ! Msg || N <- nodes()]
    end,
    broadcast().

listen() ->
    receive
        {message, Msg} ->
            io:format("received: ~p~n", [Msg])
    end,
    listen().

% pattern for responding to messages immeadietly
%
% receive
%   {From, Msg} ->
%       From ! response


% NOTES
% Some kind of main/central module
%
% keeping track of state
% loop(State) ->
%   loop(ModifiedState).
%
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
