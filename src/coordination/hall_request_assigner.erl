
-module(hall_request_assigner).

-include("../../include/worldstate.hrl").

-export([assign/1, test/0]).

%-----------------------------
% Tester hall_request_assigner for debugging bare
test() ->
    Elevator = {node(), #elevator{cabRequests=[false, false, false, false], floor=1}},
    HallRequests = [{#hallRequest{}, #hallRequest{}},
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

    JsonState = "'{\"hallRequests\": " ++
    json(lists:map(fun({HallUp, HallDown}) -> [HallUp#hallRequest.state =:= accepted, HallDown#hallRequest.state =:= accepted] end, HallRequests))

    ++ ", \"states\" : {" ++
    lists:foldr(fun(Elev, Acc) ->
                        Acc ++ ", " ++
                        elevator_to_json(Elev) end,
                    elevator_to_json(lists:nth(1, ActiveElevators)),
                    lists:nthtail(1, ActiveElevators))
    ++ "}}'",

    io:fwrite(JsonState ++ "~n"),

    Data = jsone:decode(list_to_binary(os:cmd("./apps/hall_request_assigner_mac -i " ++ JsonState))),
    maps:get(list_to_binary(atom_to_list(node())), Data).


% JSONifiers

elevator_to_json({Id, Elev}) ->
    "\"" ++ atom_to_list(Id) ++ "\" : {" ++
    "\"behaviour\": \"" ++ atom_to_list(Elev#elevator.behaviour) ++
    "\", \"floor\": " ++ integer_to_list(Elev#elevator.floor) ++
    ", \"direction\": \"" ++ atom_to_list(Elev#elevator.direction) ++
    "\", \"cabRequests\": " ++ json(Elev#elevator.cabRequests) ++
    "}".

json(Data) ->
    binary_to_list(jsone:encode(Data)).

