-module(watchdog).

-export([start/1]).

-define(TIMEOUT, 10).

start(HallRequestState) -> 
    register(watchdog, 
        spawn(fun() -> watchdog(HallRequestState, lists:duplicate(length(HallRequestState), [0,0]), []) end)
    ).

watchdog(HallRequestStates, HallRequestTimes, AssignedElevators) ->
    receive
        {watch_hall_requests, _HallRequestStates, _AssignedElevators} ->
            Diff = [(New == Old) and (New == accepted) || {New, Old} <- lists:zip(lists:flatten(_HallRequestStates), lists:flatten(HallRequestStates))],
            _HallRequestTimes = update_times(Diff, HallRequestTimes),
            % _HallRequestTimeouts = list_to_tuples(timed_out(_HallRequestTimes)),
            % TimedOutElevators=find_timed_out_elevators(_HallRequestTimeouts, AssignedElevators),
            % [{watchdog, N} ! {kill} || N <- TimedOutElevators],
            watchdog(_HallRequestStates, _HallRequestTimes, _AssignedElevators);
        {kill} ->
            io:fwrite("Timed out, got killed. ~p~n.", [self()]),
            ok
    end,
    HallRequestTimeouts = list_to_tuples(timed_out(HallRequestTimes)),
    TimedOutElevators = find_timed_out_elevators(HallRequestTimeouts, AssignedElevators),
    [{watchdog, N} ! {kill} || N <- TimedOutElevators],
    watchdog(HallRequestStates, HallRequestTimes, AssignedElevators).

update_times([], []) -> [];
update_times([true|StateTail], [TimeHead|TimeTail]) -> [TimeHead|update_times(StateTail, TimeTail)];
update_times([false|StateTail], [_|TimeTail]) -> [get_time()|update_times(StateTail, TimeTail)].

timed_out([]) -> [];
timed_out([TimeHead|TimeTail]) -> [TimeHead+?TIMEOUT < get_time()|timed_out(TimeTail)].

list_to_tuples([]) -> [];
list_to_tuples([HeadUp,HeadDown|Tail]) -> [[HeadUp,HeadDown]|Tail].

find_timed_out_elevators(_, []) -> [];
find_timed_out_elevators(HallRequestTimeouts, [{ID, Assigned}|ElevatorTail]) ->
    HasTimedOut = check_timed_out_requests(Assigned, HallRequestTimeouts),
    if
        HasTimedOut -> [ID] ++ find_timed_out_elevators(HallRequestTimeouts, ElevatorTail);
        true -> [] ++ find_timed_out_elevators(HallRequestTimeouts, ElevatorTail)
    end.

check_timed_out_requests(Assigned, TimedOut) ->
    lists:any(fun({A,B}) -> (A == B) and B end, lists:zip(lists:flatten(Assigned), lists:flatten(TimedOut))).

get_time() -> 
    {Big, Small, _} = erlang:now(),
    (Big * 1000000 + Small).