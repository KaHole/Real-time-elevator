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

merge_requests(#hallRequest{state=new}, #hallRequest{state=accepted} = HallRequest2) -> HallRequest2;

merge_requests(#hallRequest{state=accepted}, #hallRequest{state=done} = HallRequest2) -> HallRequest2;

% TODO: Experimenterer med denne! tror den er fornuftig når man tenker skikkelig gjennom
merge_requests(#hallRequest{state=done}, #hallRequest{state=nothing} = HallRequest2) -> HallRequest2;

% Kanskje, tenk nøye gjennom
% DENNE SKAL VEKK! PROBLEMET FIKSES NÅR PROBLEMET MED AT DEN Åpner døren to ganger fikses!!
% TODO: DENNE DERIMOT ER SKETCHY! hva om en kommer inn når det skal bli enighet om done!:
merge_requests(#hallRequest{state=nothing} = HallRequest1, #hallRequest{state=done}) -> HallRequest1;

% TODO: Tror denne blir riktig:
merge_requests(#hallRequest{state=accepted} = HallRequest1, #hallRequest{state=accepted}) -> HallRequest1;

merge_requests(#hallRequest{state=State1, observedBy=ObservedBy1} = HallRequest1,
               #hallRequest{state=State2, observedBy=ObservedBy2}) ->

    % TODO: Likhetsjekker kan gjøres i arg-listen, dersom args har samme navn! State,   State.. kult

    _ObservedBy = if
        State1 =:= State2 ->
            ObsBySet1 = sets:from_list(ObservedBy1),
            ObsBySet2 = sets:from_list(ObservedBy2),
            sets:to_list(sets:union(ObsBySet1, ObsBySet2));
        true -> ObservedBy1
    end,
    HallRequest1#hallRequest{observedBy=_ObservedBy}.


consense_floor({HallUp, HallDown}) ->
    {consense_request(HallUp), consense_request(HallDown)}.

consense_request(#hallRequest{state=nothing} = HallRequest) -> HallRequest;

%TODO: Tror bare den trenger dette:
consense_request(#hallRequest{state=accepted} = HallRequest) -> HallRequest;
%consense_request(#hallRequest{state=accepted, observedBy=ObservedBy}) ->
%
%    _ObservedBy = observe(ObservedBy, node()),
%    #hallRequest{state=accepted, observedBy=_ObservedBy};

consense_request(#hallRequest{state=State, observedBy=ObservedBy}) ->

    _ObservedBy = observe(ObservedBy, node()),
    Nodes = nodes(),

    % TODO: Pass på at noder som ikke lenger er aktiv, ikke blir tatt med i beregningen her? det er en svakhet med å bare ta opptelling
    if
        length(Nodes) + 1 == length(_ObservedBy) ->
            #hallRequest{state=advance(State)};
            %case State of
            %    done -> #hallRequest{};
            %    _ -> #hallRequest{state=advance(State), observedBy=[node()]}
            %end;
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
