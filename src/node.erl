-module(node).
-export([start/0, receive_msg/0, broadcast/1]).

start() ->
    % erlang:set_cookie(self(), Cookie),
    register(networking, spawn(node, receive_msg, [])),
    io:format("me: ~p~n", [node()]).

% connect to a node:
% net_kernel:connect_node(Node).

% Send message to all nodes
broadcast(Msg) ->
    [{networking, N} ! Msg || N <- nodes()],
    ok.

% Receive loop
receive_msg() ->
    receive
        {message, Msg} ->
            io:format("received: ~p~n", [Msg]);
    end,
    receive_msg().

