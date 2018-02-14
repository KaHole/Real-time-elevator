-module(node).
-export([start/0, receive_msg/0]).

% Denne kan flyttes ut til øverste nivå.. eller i app configen
-define(COOKIE, "bananpose_999").
    % erlang:set_cookie(self(), ?COOKIE),

start() ->
    register(networking, spawn(node, listen, [])),
    io:format("me: ~p~n", [node()]),
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
