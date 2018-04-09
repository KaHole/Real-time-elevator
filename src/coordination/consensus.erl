-module(consensus).
-include("../../include/worldstate.hrl").
-export([consense/2, merge_hall_request_lists/2, test/0]).


test() ->
    consense([{#hallRequest{state=nothing}, #hallRequest{state=nothing}}], [{#hallRequest{state=new, observedBy=[node()]}, #hallRequest{state=nothing}}]).

%--------------

consense(HallRequests, ExternalHallRequests) ->

    MergedHallRequests = merge_hall_request_lists(HallRequests, ExternalHallRequests),
    lists:map(fun(F) -> consense_floor(F) end, MergedHallRequests).

merge_hall_request_lists(HallRequests, ExternalHallRequests) ->
    lists:map(fun({Floor1, Floor2}) -> merge_floors(Floor1, Floor2) end, lists:zip(HallRequests, ExternalHallRequests)).

merge_floors({HallUp1, HallDown1}, {HallUp2, HallDown2}) ->
    {merge_requests(HallUp1, HallUp2), merge_requests(HallDown1, HallDown2)}.

merge_requests(#hallRequest{state=nothing} = HallRequest1, #hallRequest{state=nothing}) -> HallRequest1;

merge_requests(#hallRequest{state=nothing}, #hallRequest{state=new} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{state=new}, #hallRequest{state=accepted} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{state=accepted}, #hallRequest{state=done} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{observedBy=ObservedBy1} = HallRequest1, #hallRequest{observedBy=ObservedBy2}) ->

    ObsBySet1 = sets:from_list(ObservedBy1),
    ObsBySet2 = sets:from_list(ObservedBy2),
    _ObservedBy = sets:to_list(sets:union(ObsBySet1, ObsBySet2)),
    HallRequest1#hallRequest{observedBy=_ObservedBy}.


consense_floor({HallUp, HallDown}) ->
    {consense_request(HallUp), consense_request(HallDown)}.

consense_request(#hallRequest{state=nothing} = HallRequest) -> HallRequest;

consense_request(#hallRequest{state=State, observedBy=ObservedBy}) ->

    _ObservedBy = observe(ObservedBy, node()),
    Nodes = nodes(),

    % TODO: Pass på at noder som ikke lenger er aktiv, ikke blir tatt med i beregningen her? det er en svakhet med å bare ta opptelling
    if
        length(Nodes) + 1 == length(_ObservedBy) -> 
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
advance(done) -> nothing;
advance(S) -> S.