-module(discover).
-export([start/0]).


start() ->
    % binary | list   ... list might be really useful!
    % {ok, Socket} = gen_udp:open(8889, [binary, {broadcast, true}]),
    {ok, Socket} = gen_udp:open(8889, [binary, {active, true}]),
    listen(Socket).


% Bruk   192.168.1.255:8889  for broadcast sending!


listen(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} ->
            io:format("Klient Mottat: ~p~n", [Bin])
    end,
    listen(Socket).
