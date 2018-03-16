-module(consensus).

-include("../../include/worldstate.hrl").

-export([consense/1, test/0]).


test() ->
    consense([{#hallRequest{state=new}, #hallRequest{state=nothing}}]).


consense(HallRequests) ->
    lists:map(fun(F) -> consenseFloor(F) end, HallRequests).

consenseFloor({HallUp, HallDown}) ->
    {consenseRequest(HallUp), consenseRequest(HallDown)}.


consenseRequest(#hallRequest{state=nothing} = HallRequest) -> HallRequest;

consenseRequest(#hallRequest{state=State, observedBy=ObservedBy}) ->

    _ObservedBy = observe(ObservedBy, node()),
    Nodes = nodes(),

    if
        length(Nodes) == length(_ObservedBy) -> 
            #hallRequest{state=advance(State), observedBy=[node()]};
        true ->
            #hallRequest{state=State, observedBy=_ObservedBy}
    end.

observe(ObservedBy, Node) ->
    case lists:member(Node, ObservedBy) of
        false -> ObservedBy ++ [Node];
        true -> ObservedBy
    end.

advance(new) -> accepted;
% tror kanskje vi trenger denne allikavel:
% advance(done) -> nothing;

advance(S) -> S.
