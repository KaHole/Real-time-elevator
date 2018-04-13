-module(coordinator).
-include("../../include/worldstate.hrl").
-export([start/1]).

start({Elevators, HallRequests}) ->
    register(coordinator, spawn(fun() -> observe(Elevators, HallRequests) end)).

observe(Elevators, HallRequests) ->
    % Give priority to local_elevator-updates
    receive
        {local_elevator_update, Elevator, HallCalls} ->
            {_Elevators, _HallRequests} = handle_local_elevator_update({Elevators, HallRequests}, Elevator, HallCalls)

        after 0 -> receive
            {local_elevator_update, Elevator, HallCalls} ->
                {_Elevators, _HallRequests} = handle_local_elevator_update({Elevators, HallRequests}, Elevator, HallCalls);

            {elevator_update, Id, Elevator, ExternalHallRequests} ->

                %io:fwrite("foreign elevator ~n"),
                _Elevators = update_elevator(Elevators, Id, Elevator),
                _HallRequests = consensus:consense(HallRequests, ExternalHallRequests),

                %io:format("~p~n", [ExternalHallRequests]),
                io:format("~p~n", [_HallRequests]),

                % Send hall-requests to turn on/off the order lights
                state_poller ! {set_hall_order_button_lights, _HallRequests},

                % Get only the states of the hall-requests for comparison (change detection)
                HallRequestStates = map_hall_request_state(HallRequests),
                _HallRequestStates = map_hall_request_state(_HallRequests),

                

                % TODO: Is this enough redundancy?? The other one leads to the issue of getting an order again immeadiatly because the consenus is so fast, THIS CAN BE FIXED!
                if
                    _HallRequestStates =/= HallRequestStates ->
                    %_HallRequests =/= HallRequests ->

                        RedundantDoneHallRequests = detect_done_advancements(HallRequests, _HallRequests),

                        {_, LocalElevator} = lists:keyfind(node(), 1, Elevators),
                        % io:format("~p~n", [_HallRequestStates]),
                        broadcast_state(LocalElevator, RedundantDoneHallRequests);
                    true -> ok
                end,

                AssignedHallCalls = hall_request_assigner:assign(_Elevators, _HallRequestStates),

                % Sends assigned hall-requests to elevator logic
                state_poller ! {set_hall_calls, AssignedHallCalls}
        end
    end,

    observe(_Elevators, _HallRequests).

handle_local_elevator_update({Elevators, HallRequests}, Elevator, HallCalls) ->
    % io:fwrite("local elevator ~n"),
    _Elevators = update_elevator(Elevators, node(), Elevator),
    _HallRequests = update_hall_requests(HallRequests, HallCalls),

    %TODO: If 1 ELEVATOR IS RUN; we could consense her isntead of doing messaging, but its a simple solution, so if it works ...
    % If the elevator is alone, invoke consensus with self by sending a foreign elevator update to itself.
    Nodes = nodes(),
    if length(Nodes) == 0 -> coordinator ! {elevator_update, node(), Elevator, _HallRequests}; true -> ok end,

    % Send hall-requests to turn on/off the order lights
    state_poller ! {set_hall_order_button_lights, _HallRequests},

    broadcast_state(Elevator, _HallRequests),
    {_Elevators, _HallRequests}.


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

map_hall_request_state(HallRequests) ->
    lists:map(
        fun({HallUp, HallDown}) ->
            {HallUp#hallRequest.state, HallDown#hallRequest.state}
        end, HallRequests).

detect_done_advancements([], []) -> [];

% TODO: Add comments explaining
detect_done_advancements([{#hallRequest{state=accepted} = HallUp, _} | Tail], [{#hallRequest{state=nothing}, NewHallDown} | NewTail]) ->
    [{HallUp#hallRequest{state=done, observedBy = HallUp#hallRequest.observedBy ++ nodes()},
    NewHallDown}] ++ detect_done_advancements(Tail, NewTail);

detect_done_advancements([{#hallRequest{state=done} = HallUp, _} | Tail], [{#hallRequest{state=nothing}, NewHallDown} | NewTail]) ->
    [{HallUp#hallRequest{observedBy = HallUp#hallRequest.observedBy ++ nodes()},
    NewHallDown}] ++ detect_done_advancements(Tail, NewTail);

detect_done_advancements([{_, #hallRequest{state=accepted} = HallDown} | Tail], [{NewHallUp, #hallRequest{state=nothing}} | NewTail]) ->
    [{NewHallUp,
    HallDown#hallRequest{state=done, observedBy = HallDown#hallRequest.observedBy ++ nodes()} }]
    ++ detect_done_advancements(Tail, NewTail);

detect_done_advancements([{_, #hallRequest{state=done} = HallDown} | Tail], [{NewHallUp, #hallRequest{state=nothing}} | NewTail]) ->
    [{NewHallUp,
    HallDown#hallRequest{observedBy = HallDown#hallRequest.observedBy ++ nodes()} }]
    ++ detect_done_advancements(Tail, NewTail);

detect_done_advancements([_ | Tail], [NewHallRequest | NewTail]) ->
    [NewHallRequest] ++ detect_done_advancements(Tail, NewTail).


broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].
