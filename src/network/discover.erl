-module(discover).
-export([start/0]).

-define(DISCOVER_PORT, 8889).

% Husk at PCene som testes må ha samme erlang cookie, denne kan sendes inn som argument! så mindre hassle med filene osv

start() ->
    {ok, Socket} = gen_udp:open(?DISCOVER_PORT, [list, {active, true}, {reuseaddr, true}, {broadcast, true}]),
    Listener = spawn(fun() -> listen(Socket) end),
    gen_udp:controlling_process(Socket, Listener),
    spawn(fun() -> broadcast(Socket, nodes()) end).

broadcast(Socket, []) ->
    % gen_udp:send(Socket, {192,168,1,255}, ?DISCOVER_PORT, atom_to_list(node(self()))),
    % gen_udp:send(Socket, {172,20,10,15}, ?DISCOVER_PORT, atom_to_list(node(self()))),

    gen_udp:send(Socket, {129,241,187,255}, ?DISCOVER_PORT, atom_to_list(node(self()))),

    timer:sleep(1000),
    broadcast(Socket, nodes());

broadcast(_, _) -> ok.

% Gode ideer her:
% Evig loop, starter broadcast igjen dersom node-listen tømmes
% broadcast(Socket, _) ->
%     timer:sleep(10000), % timeout nesten
%     broadcast(Socket, nodes()).

listen(Socket) ->
    receive
        {udp, Socket, _, _, List} ->
            io:format("Discovered ~p~n", [List]),
            % NB! Her kan vi sende world-state med en gang, slik at når en node kommer tilbake så får den staten til alle med en gang.
            % kan bare erstatte connect med en melding i stedet, connect er bare en "tom" melding

            % TODO: Dont connect to self! ..?

            DiscoveredNode = list_to_atom(List),

            case node() =:= DiscoveredNode of
                false ->
                    io:format("connecting to ~p~n", [List]),
                    % net_kernel:connect_node(DiscoveredNode),
                    {test, DiscoveredNode} ! ping;
                true -> ok
            end,
            io:format("Nodes: ~p~n", [nodes()])
    end,
    listen(Socket).
