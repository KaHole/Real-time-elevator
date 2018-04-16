-module(hall_request_assigner).
-include("../../include/worldstate.hrl").
-export([assign/2]).

assign(Elevators, HallRequestStates) ->

    HasAcceptedHallRequests = lists:any(fun([HallUp, HallDown]) ->
        (HallUp =:= accepted) or (HallDown =:= accepted) end, HallRequestStates),

    if 
       HasAcceptedHallRequests ->
            % Filters on active nodes, only active nodes should be taken in account
            ActiveNodes = nodes() ++ [node()],
            ActiveElevators = lists:filter(fun({Id, _}) -> lists:member(Id, ActiveNodes) end, Elevators),

            ElevatorJson = lists:foldr(fun(Elev, Acc) ->
                                Acc ++ ", " ++
                                elevator_to_json(Elev) end,
                            elevator_to_json(lists:nth(1, ActiveElevators)),
                            lists:nthtail(1, ActiveElevators)),

            JsonState = io_lib:format("'{\"hallRequests\": ~s, \"states\": {~s}}'", [hall_requests_to_json(HallRequestStates), ElevatorJson]),

            Data = jsone:decode(list_to_binary(os:cmd("./apps/hall_request_assigner -i " ++ JsonState))),
            {maps:get(list_to_binary(atom_to_list(node())), Data),
                lists:map(fun({Id, Assigned}) -> {list_to_atom(binary_to_list(Id)), Assigned} end, maps:to_list(Data))};

        true -> {lists:duplicate(length(HallRequestStates), [false, false]), []}
    end.

hall_requests_to_json(HallRequestStates) ->
    json(lists:map(fun([HallUp, HallDown]) ->
                           [HallUp =:= accepted, HallDown =:= accepted] end,
                   HallRequestStates)).

elevator_to_json({Id, #elevator{behaviour=Behaviour, floor=Floor, direction=Dir, cabCalls=CabCalls}}) ->
    io_lib:format("\"~s\" : {\"behaviour\": \"~s\", \"floor\": ~p, \"direction\": \"~s\", \"cabRequests\": ~s}",
                  [Id, Behaviour, Floor, Dir, json(CabCalls)]).

json(Data) -> binary_to_list(jsone:encode(Data)).
