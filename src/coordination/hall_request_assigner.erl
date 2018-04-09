-module(hall_request_assigner).
-include("../../include/worldstate.hrl").
-export([assign/1, test/0]).

%-----------------------------
% Tester hall_request_assigner for debugging bare
test() ->
    Elevator = {node(), #elevator{cabRequests=[false, false, false, false], floor=1}},
    HallRequests = [{#hallRequest{state=accepted}, #hallRequest{}},
                    {#hallRequest{}, #hallRequest{}},
                    {#hallRequest{}, #hallRequest{}},
                    {#hallRequest{}, #hallRequest{}}],
    Elevators = [Elevator],
    assign({Elevators, HallRequests}).
    % jsone:encode([false,false,false,false]).

%-----------------------------

assign({Elevators, HallRequests}) ->

    % Filters on active nodes, only active nodes should be taken in account
    ActiveNodes = nodes() ++ [node()],
    ActiveElevators = lists:filter(fun({Id, _}) -> lists:member(Id, ActiveNodes) end, Elevators),

    ElevatorJson = lists:foldr(fun(Elev, Acc) ->
                        Acc ++ ", " ++
                        elevator_to_json(Elev) end,
                    elevator_to_json(lists:nth(1, ActiveElevators)),
                    lists:nthtail(1, ActiveElevators)),

    JsonState = io_lib:format("'{\"hallRequests\": ~s, \"states\": {~s}}'", [hall_requests_to_json(HallRequests), ElevatorJson]),

    Data = jsone:decode(list_to_binary(os:cmd("./apps/hall_request_assigner -i " ++ JsonState))),
    maps:get(list_to_binary(atom_to_list(node())), Data).


hall_requests_to_json(HallRequests) ->
    json(lists:map(fun({HallUp, HallDown}) ->
                           [HallUp#hallRequest.state =:= accepted, HallDown#hallRequest.state =:= accepted] end,
                   HallRequests)).

elevator_to_json({Id, #elevator{behaviour=Behaviour, floor=Floor, direction=Dir, cabRequests=CabRequests}}) ->
    io_lib:format("\"~s\" : {\"behaviour\": \"~s\", \"floor\": ~p, \"direction\": \"~s\", \"cabRequests\": ~s}",
                  [Id, Behaviour, Floor, Dir, json(CabRequests)]).

json(Data) ->
    binary_to_list(jsone:encode(Data)).
