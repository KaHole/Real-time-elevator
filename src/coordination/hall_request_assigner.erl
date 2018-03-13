
-module(hall_request_assigner).

-include("../../include/worldstate.hrl").

-export([assign/1, test/0]).

%-----------------------------
% Tester hall_request_assigner for debugging bare
test() ->
    Elevator = {node(), #elevator{cabRequests=[false, false, false, false], floor=1}},
    WorldState = #worldState{hallRequests=[{#hallRequest{}, #hallRequest{}},
                                           {#hallRequest{}, #hallRequest{}},
                                           {#hallRequest{}, #hallRequest{}},
                                           {#hallRequest{}, #hallRequest{}}], elevators=[Elevator]},
    assign(WorldState).

%-----------------------------

assign(WorldState) ->

    JsonState = "'{\"hallRequests\": [" ++
    lists:foldr(fun(Hall, Acc) ->
                        Acc ++ ", " ++
                        hall_to_json(Hall) end,
                    hall_to_json(lists:nth(1, WorldState#worldState.hallRequests)), % First element in list
                    lists:nthtail(1, WorldState#worldState.hallRequests))

    ++ "], \"states\" : {" ++
    lists:foldr(fun(Elev, Acc) ->
                        Acc ++ ", " ++
                        elevator_to_json(Elev) end,
                    elevator_to_json(lists:nth(1, WorldState#worldState.elevators)),
                    lists:nthtail(1, WorldState#worldState.elevators))
    ++ "}}'",

    % io:fwrite(JsonState ++ "~n"),

    Data = jsone:decode(list_to_binary(os:cmd("./apps/hall_request_assigner_mac -i " ++ JsonState))),
    maps:get(list_to_binary(atom_to_list(node())), Data).


% JSONifiers

bs(true) -> "true";
bs(_) -> "false".

hall_to_json({HallRequest1, HallRequest2}) ->
    "[" ++ bs(HallRequest1#hallRequest.state =:= accepted) ++
    "," ++ bs(HallRequest2#hallRequest.state =:= accepted) ++ "]".

elevator_to_json({Id, Elev}) ->
    "\"" ++ atom_to_list(Id) ++ "\" : {" ++
    "\"behaviour\": \"" ++ atom_to_list(Elev#elevator.behaviour) ++
    "\", \"floor\": " ++ integer_to_list(Elev#elevator.floor) ++
    ", \"direction\": \"" ++ atom_to_list(Elev#elevator.direction) ++
    "\", \"cabRequests\": [" ++
    lists:foldr(fun(Cab, CAcc) -> CAcc ++ ", " ++ bs(Cab) end,
                bs(lists:nth(1, Elev#elevator.cabRequests)),
                lists:nthtail(1, Elev#elevator.cabRequests))
    ++ "]}".
