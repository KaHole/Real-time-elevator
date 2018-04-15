-module(discover).
-export([start/0]).

-define(DISCOVER_PORT, 28960).
-define(DISCOVER_RATE, 1000).

start() ->
    {ok, Socket} = gen_udp:open(?DISCOVER_PORT, [list, {active, true}, {reuseaddr, true}, {broadcast, true}]),
    Listener = spawn(fun() -> listen(Socket) end),
    gen_udp:controlling_process(Socket, Listener),
    register(discover, spawn(fun() -> broadcast(Socket, nodes()) end)).

broadcast(Socket, []) ->
    %gen_udp:send(Socket, {172,20,10,15}, ?DISCOVER_PORT, atom_to_list(node(self()))),
    % gen_udp:send(Socket, {129,241,187,255}, ?DISCOVER_PORT, atom_to_list(node(self()))),
    gen_udp:send(Socket, {10,22,39,255}, ?DISCOVER_PORT, atom_to_list(node(self()))),

    timer:sleep(?DISCOVER_RATE),
    broadcast(Socket, nodes());

% Evig loop, starter broadcast igjen dersom node-listen tømmes
broadcast(Socket, _) ->
    timer:sleep(?DISCOVER_RATE),
    broadcast(Socket, nodes()).

listen(Socket) ->
    receive
        {udp, Socket, _, _, List} ->
            DiscoveredNode = list_to_atom(List),

            case node() =:= DiscoveredNode of
                false ->
                    io:format("Discovered ~p~n", [List]),
                    io:format("connecting to ~p~n", [List]),
                    % net_kernel:connect_node(DiscoveredNode),
                    {connect, DiscoveredNode} ! ping,
                    io:format("Nodes: ~p~n", [nodes()]);
                _ -> ok
            end
    end,
    listen(Socket).