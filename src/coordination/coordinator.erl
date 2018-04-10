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
            _HallRequests = update_hall_requests(HallRequests, HallCalls),

            %TODO: Assign hall_requests here????? PROBABLY NO POINT

            broadcast_state(Elevator, _HallRequests);

        {elevator_update, Id, Elevator, ExternalHallRequests} ->

            %io:fwrite("foreign elevator ~n"),
            _Elevators = update_elevator(Elevators, Id, Elevator),
            _HallRequests = consensus:consense(HallRequests, ExternalHallRequests),

            % Check for changes? otherwise we get SPAM!
            io:fwrite("---------------------------------~n"),
            io:format("~p~n", [HallRequests]),
            io:format("~p~n", [_HallRequests]),
            io:fwrite("---------------------------------~n"),
            if
                _HallRequests =/= HallRequests ->
                        io:fwrite("HallRequest changes above!! ~n"),
                        io:fwrite("---------------------------------~n"),
                        io:fwrite("---------------------------------~n"),
                    {_, LocalElevator} = lists:keyfind(node(), 1, Elevators),
                    broadcast_state(LocalElevator, _HallRequests);
                true -> ok
            end,

            %TODO: If no hall-requests, this should return imeadietly with an [[false, false], .... ]
            AssignedHallCalls = hall_request_assigner:assign({_Elevators, _HallRequests}),

            % Sends assigned hall-requests to elevator logic
            state_poller ! {set_hall_calls, AssignedHallCalls}
    end,
    observe(_Elevators, _HallRequests).


update_elevator(Elevators, Id, Elevator) ->
    lists:keystore(Id, 1, Elevators, {Id, Elevator}).

update_hall_requests(HallRequests, HallCalls) ->

    NewHallRequests = lists:map(fun([HallUp, HallDown]) ->
                                        {generate_hall_request(HallUp), generate_hall_request(HallDown)}
                                end, HallCalls),

    % Merge with existing in the same way as foreign hallrequests, this is a clever way of doing it.
    consensus:merge_hall_request_lists(HallRequests, NewHallRequests).

generate_hall_request(done) -> #hallRequest{state=done, observedBy=[node()]};
generate_hall_request(true) -> #hallRequest{state=new, observedBy=[node()]};
generate_hall_request(false) -> #hallRequest{}.

broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].
