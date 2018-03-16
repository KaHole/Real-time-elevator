-module(coordinator).
-include("../../include/worldstate.hrl").
-export([start/1]).

start(WorldState) ->
    register(coordinator, spawn(fun() -> observe(WorldState) end)).

% BIBELEN:
% WHEN CALLING HALL_REQUEST_ASSIGNER, FILTER ON nodes() TO ONLY SEND THE ACTIVE ONES!!

% trenger kanskje ikke worldState record.. mer rot bare? hmm 50-50
observe(#worldState{elevators=Elevators, hallRequests=HallRequests}) ->

    receive
        {local_elevator_update, Elevator, HallButtons} ->

            _Elevators = update_elevator(Elevators, node(), Elevator),
            _HallRequests = add_hall_requests(HallRequests, HallButtons),

            broadcast_state(Elevator, _HallRequests);

        {elevator_update, Id, Elevator, NewHallRequests} ->

            io:fwrite("foreign elevator ~n"),

            _Elevators = update_elevator(Elevators, Id, Elevator),

            %TODO: Maybe put more of this stuff in the consense module, makes it a lot tidier here as well.
            _ChangeThis_HallRequests = update_hall_requests(HallRequests, NewHallRequests),

            _HallRequests = consensus:consense(_ChangeThis_HallRequests),

            {_, LocalElevator} = lists:keyfind(node(), 1, Elevators),
            broadcast_state(LocalElevator, _HallRequests),

            %TODO: prune/filter on active nodes! only active nodes should be taken in account
            AssignedHallRequests = hall_request_assigner:assign(_HallRequests),

            % Send assigned hall-requests to elevator logic
            elevator_logic ! {hall_requests, AssignedHallRequests}
    end,

    observe(#worldState{elevators=_Elevators, hallRequests=_HallRequests}).


update_elevator(Elevators, Id, Elevator) ->
    lists:keyreplace(Id, 1, Elevators, {Id, Elevator}).

add_hall_requests(HallRequests, HallButtons) ->

                                    %TODO: Warning! this might need to be square brackets, same behaviour, doesnt matter really, but watch out
    NewHallRequests = lists:map(fun({HallUp, HallDown}) ->
                                        {generate_hall_request(HallUp), generate_hall_request(HallDown)}
                                end, HallButtons),

    % Merge with existing in the same way as foreign hallrequests, this is a clever way of doing it.
    update_hall_requests(HallRequests, NewHallRequests).


generate_hall_request(true) -> #hallRequest{state=new, observedBy=[node()]};
generate_hall_request(false) -> #hallRequest{}.


% Move some of this to consensus perhaps? seems really related
%
update_hall_requests([{HallUp, HallDown} | Tail], [{NewHallUp, NewHallDown} | NewTail]) ->
    [{merge_hall_requests(HallUp, NewHallUp), merge_hall_requests(HallDown, NewHallDown)} | update_hall_requests(Tail, NewTail)];

update_hall_requests([], []) -> [].

merge_hall_requests(#hallRequest{state=nothing} = HallRequest1, #hallRequest{state=nothing}) -> HallRequest1;

merge_hall_requests(#hallRequest{state=nothing}, #hallRequest{state=new} = HallRequest2) -> HallRequest2;

merge_hall_requests(#hallRequest{state=new}, #hallRequest{state=accepted} = HallRequest2) -> HallRequest2;

merge_hall_requests(#hallRequest{observedBy=ObservedBy1} = HallRequest1, #hallRequest{observedBy=ObservedBy2}) ->

    ObsBySet1 = sets:from_list(ObservedBy1),
    ObsBySet2 = sets:from_list(ObservedBy2),
    _ObservedBy = sets:to_list(sets:union(ObsBySet1, ObsBySet2)),

    HallRequest1#hallRequest{observedBy=_ObservedBy}.


broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].
