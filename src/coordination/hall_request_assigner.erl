-module(hall_request_assigner).
-include("../../include/worldstate.hrl").
-export([assign/1, test/0]).

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

elevator_to_json({Id, #elevator{behaviour=Behaviour, floor=Floor, direction=Dir, cabCalls=CabCalls}}) ->
    io_lib:format("\"~s\" : {\"behaviour\": \"~s\", \"floor\": ~p, \"direction\": \"~s\", \"cabRequests\": ~s}",
                  [Id, Behaviour, Floor, Dir, json(CabCalls)]).

json(Data) -> binary_to_list(jsone:encode(Data)).