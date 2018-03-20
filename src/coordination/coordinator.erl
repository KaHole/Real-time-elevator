-module(coordinator).
-include("../../include/worldstate.hrl").
-export([start/1]).

start({Elevators, HallRequests}) ->
    register(coordinator, spawn(fun() -> observe(Elevators, HallRequests) end)).

observe(Elevators, HallRequests) ->
    receive
        {local_elevator_update, Elevator, HallCalls} ->

            io:fwrite("local elevator ~n"),
            _Elevators = update_elevator(Elevators, node(), Elevator),
            _HallRequests = add_hall_requests(HallRequests, HallCalls),
            broadcast_state(Elevator, _HallRequests);

        {elevator_update, Id, Elevator, ExternalHallRequests} ->

            io:fwrite("foreign elevator ~n"),

            _Elevators = update_elevator(Elevators, Id, Elevator),
            _HallRequests = consensus:consense(HallRequests, ExternalHallRequests),

            % TODO: is this local elevator data redundant / stale.. does it matter?
            {_, LocalElevator} = lists:keyfind(node(), 1, Elevators),
            broadcast_state(LocalElevator, _HallRequests),

            AssignedHallCalls = hall_request_assigner:assign({_Elevators, _HallRequests}),

            % Send assigned hall-requests to elevator logic
            elevator_logic ! {hall_calls, AssignedHallCalls}
    end,
    observe(_Elevators, _HallRequests).


update_elevator(Elevators, Id, Elevator) ->
    lists:keystore(Id, 1, Elevators, {Id, Elevator}).

add_hall_requests(HallRequests, HallCalls) ->

            %TODO: Warning! this might need to be square brackets, same behaviour, doesnt matter really, but watch out
    NewHallRequests = lists:map(fun({HallUp, HallDown}) ->
                                        {generate_hall_request(HallUp), generate_hall_request(HallDown)}
                                end, HallCalls),

    % Merge with existing in the same way as foreign hallrequests, this is a clever way of doing it.
    consensus:merge_hall_request_lists(HallRequests, NewHallRequests).

generate_hall_request(true) -> #hallRequest{state=new, observedBy=[node()]};
generate_hall_request(false) -> #hallRequest{}.

broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].
