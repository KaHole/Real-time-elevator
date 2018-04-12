-module(coordinator).
-include("../../include/worldstate.hrl").
-export([start/1]).

start({Elevators, HallRequests}) ->
    register(coordinator, spawn(fun() -> observe(Elevators, HallRequests) end)).

observe(Elevators, HallRequests) ->
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

                % Send hall-requests to turn on/off the order lights
                state_poller ! {set_hall_order_button_lights, _HallRequests},

                % Get only the states of the hall-requests for comparison (change detection)
                HallRequestStates = map_hall_request_state(HallRequests),
                _HallRequestStates = map_hall_request_state(_HallRequests),

                if
                    _HallRequestStates =/= HallRequestStates ->
                        {_, LocalElevator} = lists:keyfind(node(), 1, Elevators),
                        % io:format("~p~n", [_HallRequestStates]),
                        broadcast_state(LocalElevator, _HallRequests);
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

    %TODO: Assign hall_requests and send to state_poller here????? PROBABLY NO POINT

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

broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].
