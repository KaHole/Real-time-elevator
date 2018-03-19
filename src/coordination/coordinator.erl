-module(coordinator).
-include("../../include/worldstate.hrl").
-export([start/1]).

start({Elevators, HallRequests}) ->
    register(coordinator, spawn(fun() -> observe(Elevators, HallRequests) end)).

% observe() ->

%     local_elevator_update->
%         update_elevator.
%         add_hall_requests.
%         broadcast_state.

%     elevator_update ->
%         update_elevator.
%         update_hall_requests
%         consense
%         broadcast_state
%         assign_hall_requests
%         send_hall_requests_to_local

% trenger kanskje ikke worldState record.. mer rot bare? hmm 50-50

observe(Elevators, HallRequests) ->
    receive
        {local_elevator_update, Elevator, HallButtons} ->

            io:fwrite("local elevator ~n"),
            _Elevators = update_elevator(Elevators, node(), Elevator),
            _HallRequests = add_hall_requests(HallRequests, HallButtons),
            broadcast_state(Elevator, _HallRequests);

        {elevator_update, Id, Elevator, NewHallRequests} ->

            io:fwrite("foreign elevator ~n"),

            _Elevators = update_elevator(Elevators, Id, Elevator),

            % TODO: Maybe put more of this stuff in the consense module, makes it a lot tidier here as well.
            MergedHallRequests = update_hall_requests(HallRequests, NewHallRequests),
            _HallRequests = consensus:consense(MergedHallRequests),

            % TODO: is this local elevator data redundant / stale.. does it matter?
            {_, LocalElevator} = lists:keyfind(node(), 1, Elevators),
            broadcast_state(LocalElevator, _HallRequests),

            % Filters on active nodes, only active nodes should be taken in account
            % TODO: Do this in hall_assigner instead?!!
            ActiveNodes = nodes(),
            ActiveElevators = lists:filter(fun(E) -> lists:member(E, ActiveNodes) , _Elevators),
            AssignedHallCalls = hall_request_assigner:assign({ActiveElevators, _HallRequests}),

            % Send assigned hall-requests to elevator logic
            elevator_logic ! {hall_requests, AssignedHallCalls}
    end,
    observe(_Elevators, _HallRequests).


update_elevator(Elevators, Id, Elevator) ->
    lists:keystore(Id, 1, Elevators, {Id, Elevator}).

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
