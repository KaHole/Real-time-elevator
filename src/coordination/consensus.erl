-module(consensus).
-include("../../include/worldstate.hrl").
-export([consense/2, merge_hall_request_lists/2]).

consense(HallRequests, ExternalHallRequests) ->

    MergedHallRequests = merge_hall_request_lists(HallRequests, ExternalHallRequests),
    lists:map(fun(F) -> consense_floor(F) end, MergedHallRequests).

merge_hall_request_lists(HallRequests, ExternalHallRequests) ->
    lists:map(fun({Floor1, Floor2}) -> merge_floors(Floor1, Floor2) end, lists:zip(HallRequests, ExternalHallRequests)).

merge_floors({HallUp1, HallDown1}, {HallUp2, HallDown2}) ->
    {merge_requests(HallUp1, HallUp2), merge_requests(HallDown1, HallDown2)}.

merge_requests(#hallRequest{state=nothing} = HallRequest1, #hallRequest{state=nothing}) -> HallRequest1;

merge_requests(#hallRequest{state=nothing}, #hallRequest{state=new} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{state=nothing}, #hallRequest{state=accepted} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{state=new}, #hallRequest{state=accepted} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{state=accepted}, #hallRequest{state=done} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{state=accepted} = HallRequest1, #hallRequest{state=accepted}) -> HallRequest1;

% Merge requests with same state by adding the observers together
merge_requests(#hallRequest{state=State, observedBy=ObservedBy1} = HallRequest1,
               #hallRequest{state=State, observedBy=ObservedBy2}) ->
    
    ObsBySet1 = sets:from_list(ObservedBy1),
    ObsBySet2 = sets:from_list(ObservedBy2),
    _ObservedBy = sets:to_list(sets:union(ObsBySet1, ObsBySet2)),
    HallRequest1#hallRequest{observedBy=_ObservedBy};

merge_requests(HallRequest1, _) -> HallRequest1.

consense_floor({HallUp, HallDown}) ->
    {consense_request(HallUp), consense_request(HallDown)}.

consense_request(#hallRequest{state=nothing} = HallRequest) -> HallRequest;

consense_request(#hallRequest{state=accepted} = HallRequest) -> HallRequest;

consense_request(#hallRequest{state=State, observedBy=ObservedBy}) ->

    _ObservedBy = observe(ObservedBy, node()),
    Nodes = nodes(),

    if
        length(_ObservedBy) >= length(Nodes) + 1 ->
            #hallRequest{state=advance(State)};
        true ->
            #hallRequest{state=State, observedBy=_ObservedBy}
    end.

observe(ObservedBy, Node) ->
    case lists:member(Node, ObservedBy) of
        false -> ObservedBy ++ [Node];
        _ -> ObservedBy
    end.

advance(new) -> accepted;
advance(done) -> nothing;
advance(S) -> S.