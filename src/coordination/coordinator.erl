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

        % TODO: Only fetch nodes() once per run, this is important for it to be determenistic! and purely functional,
            % and to not get strange side-effects where the node-list changes in between shit.


        % TODO: FUCK THIS? JUST USE DONE ATOM in hall-calls from elevator logic?
        %  Treat this a seperate event? Design kinda makes it impossible to do anything else.
        % {hall_request_done, Floor, Direction} ->
        %     % mark hall-request as done and SEND IT OUT!!
        %     % this will have the same stale elevator stale problem as above. doesnt really matter but, something to consider
        %     _HallRequests = mark_hall_request_done(Floor, Direction, HallRequests),
        %     _Elevators = Elevators

    end,
    observe(_Elevators, _HallRequests).


% General TODO: Pass på HallRequest vs HallCalls over alt! Samme med CabCalls (CabRequests skal ikke egentlig finnes)


update_elevator(Elevators, Id, Elevator) ->
    lists:keystore(Id, 1, Elevators, {Id, Elevator}).

update_hall_requests(HallRequests, HallCalls) ->

            %TODO: Warning! this might need to be square brackets, same behaviour, doesnt matter really, but watch out
    NewHallRequests = lists:map(fun({HallUp, HallDown}) ->
                                        {generate_hall_request(HallUp), generate_hall_request(HallDown)}
                                end, HallCalls),

    % Merge with existing in the same way as foreign hallrequests, this is a clever way of doing it.
    consensus:merge_hall_request_lists(HallRequests, NewHallRequests).


% TODO: FUCK THIS? same as above
% % TODO: Pass på indekseringen!! 0,1 wtf
% mark_hall_request_done(0, up, [{HallUp, HallDown}|Tail]) -> [{HallUp#hallRequest{status=done, observedBy=[node()]}, HallDown}|Tail];

% mark_hall_request_done(0, down, [{HallUp, HallDown}|Tail]) -> [{HallUp, HallDown#hallRequest{status=done, observedBy=[node()]}}|Tail];

% mark_hall_request_done(Floor, Direction, [Hall|Tail]) -> [Hall|mark_hall_request_done(Floor-1, Direction, Tail)]


generate_hall_request(done) -> #hallRequest{state=done, observedBy=[node()]};
generate_hall_request(true) -> #hallRequest{state=new, observedBy=[node()]};
generate_hall_request(false) -> #hallRequest{}.

broadcast_state(Elevator, HallRequests) ->
    [{coordinator, N} ! {elevator_update, node(), Elevator, HallRequests} || N <- nodes()].
