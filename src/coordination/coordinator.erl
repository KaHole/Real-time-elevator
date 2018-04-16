-module(coordinator).
-include("../../include/worldstate.hrl").
-export([start/1]).

start({Elevators, HallRequests}) ->
    register(coordinator, spawn(fun() -> observe(Elevators, HallRequests) end)).

observe(Elevators, HallRequests) ->
    % Give priority to local_elevator-updates, using the receive, after 0 pattern
    receive
        {local_elevator_update, Elevator, HallCalls} ->
            {_Elevators, _HallRequests} = handle_local_elevator_update({Elevators, HallRequests}, Elevator, HallCalls)

        after 0 -> receive
            {local_elevator_update, Elevator, HallCalls} ->
                {_Elevators, _HallRequests} = handle_local_elevator_update({Elevators, HallRequests}, Elevator, HallCalls);

            {elevator_update, Id, Elevator, ExternalHallRequests} ->

                _Elevators = update_elevator(Elevators, Id, Elevator),
                _HallRequests = handle_hall_requests({_Elevators, HallRequests}, ExternalHallRequests)

            after 2000 -> % trigger consensus periodically, so it still works even without messages
                _Elevators = Elevators,
                _HallRequests = handle_hall_requests({Elevators, HallRequests}, HallRequests)
        end
    end,

    observe(_Elevators, _HallRequests).

handle_local_elevator_update({Elevators, HallRequests}, Elevator, HallCalls) ->

    _Elevators = update_elevator(Elevators, node(), Elevator),
    _HallRequests = update_hall_requests(HallRequests, HallCalls),

    % If the elevator is alone, invoke consensus with self by sending a foreign elevator update to itself.
    Nodes = nodes(),
    if length(Nodes) == 0 -> coordinator ! {elevator_update, node(), Elevator, _HallRequests}; true -> ok end,

    % Send hall-requests to turn on/off the order lights
    state_poller ! {set_hall_order_button_lights, _HallRequests},

    broadcast_state(Elevator, _HallRequests),
    {_Elevators, _HallRequests}.

handle_hall_requests({Elevators, HallRequests}, ExternalHallRequests) ->
    _HallRequests = consensus:consense(HallRequests, ExternalHallRequests),

    % Send hall-requests to turn on/off the order lights
    state_poller ! {set_hall_order_button_lights, _HallRequests},

    % Get only the states of the hall-requests for comparison (change detection)
    HallRequestStates = map_hall_request_state(HallRequests),
    _HallRequestStates = map_hall_request_state(_HallRequests),

    if
        _HallRequestStates =/= HallRequestStates -> % Only send message if any changes
            % Adds some redundancy to the done-stage of hall-requests. (by repeating the step once)
            RedundantDoneHallRequests = detect_done_advancements(HallRequests, _HallRequests),

            {_, LocalElevator} = lists:keyfind(node(), 1, Elevators),
            broadcast_state(LocalElevator, RedundantDoneHallRequests);

        true -> ok
    end,

    {AssignedHallCalls, ElevatorDelegation} = hall_request_assigner:assign(Elevators, _HallRequestStates),

    watchdog ! {watch_hall_requests, _HallRequestStates, ElevatorDelegation},

    % Sends assigned hall-requests to elevator logic
    state_poller ! {set_hall_calls, AssignedHallCalls},
    _HallRequests.


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
            [HallUp#hallRequest.state, HallDown#hallRequest.state]
        end, HallRequests).

% Function detecting if any hall-requests became done in the last step
detect_done_advancements([], []) -> [];

detect_done_advancements([{HallUp, HallDown}|Tail], [{NewHallUp, NewHallDown}|NewTail]) ->
    UpAdvanced = done_advanced(HallUp, NewHallUp),
    DownAdvanced = done_advanced(HallDown, NewHallDown),

    _HallUp = if
        UpAdvanced ->
            #hallRequest{state=done, observedBy=HallUp#hallRequest.observedBy ++ nodes() ++ [node()]};
        true -> NewHallUp
    end,
    _HallDown = if
        DownAdvanced ->
            #hallRequest{state=done, observedBy=HallDown#hallRequest.observedBy ++ nodes() ++ [node()]};
        true -> NewHallDown
    end,
    [{_HallUp, _HallDown}] ++ detect_done_advancements(Tail, NewTail).

done_advanced(#hallRequest{state=accepted}, #hallRequest{state=nothing}) -> true;

done_advanced(#hallRequest{state=done}, #hallRequest{state=nothing}) -> true;

done_advanced(_, _) -> false.

broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].
