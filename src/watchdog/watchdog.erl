-module(watchdog).

-export([start/2]).

-define(TIMEOUT, 40).

start(HallRequestState, HallRequestTimes) -> 
    register(watchdog, 
        spawn(fun() -> watchdog(HallRequestState, HallRequestTimes) end)
    ).

watchdog(HallRequestState, HallRequestTimes) ->
    receive
        {hall_request_states, _HallRequestStates, AssignedElevators} ->
            Diff = [(New == Old) and (New == accepted) || {New, Old} <- lists:zip(lists:flatten(_HallRequestStates), lists:flatten(HallRequestState))],
            _HallRequestTimes = update_times(Diff, HallRequestTimes),
            _HallRequestTimeouts = list_to_tuples(timed_out(_HallRequestTimes)),
            TimedOutElevators=find_timed_out_elevators(_HallRequestTimeouts, AssignedElevators),
            [{watchdog, N} ! {kill} || N <- TimedOutElevators],
            watchdog(_HallRequestStates, _HallRequestTimes);
        {kill} ->
            ok
    end,
    watchdog(HallRequestState, HallRequestTimes).

update_times([], []) -> [];
update_times([true|StateTail], [TimeHead|TimeTail]) -> [TimeHead|update_times(StateTail, TimeTail)];
update_times([false|StateTail], [_|TimeTail]) -> [time()|update_times(StateTail, TimeTail)].

timed_out([]) -> [];
timed_out([TimeHead|TimeTail]) -> [TimeHead+?TIMEOUT < time()|timed_out(TimeTail)].

list_to_tuples([]) -> [];
list_to_tuples([HeadUp,HeadDown|Tail]) -> [[HeadUp,HeadDown]|Tail].

find_timed_out_elevators([], []) -> [];
find_timed_out_elevators(TimedOut, [{ID, Assigned}|ElevatorTail]) ->
    TimedOut = check_timed_out_requests(Assigned, TimedOut),
    if
        TimedOut -> [ID] ++ find_timed_out_elevators(TimedOut, ElevatorTail);
        true -> [] ++ find_timed_out_elevators(TimedOut, ElevatorTail)
    end.

check_timed_out_requests(Assigned, TimedOut) ->
    lists:any(fun({A,B}) -> (A == B) and B end, lists:zip(lists:flatten(Assigned), lists:flatten(TimedOut))).

time() -> 
    {Big, Small, _} = erlang:now(),
    (Big * 1000000 + small).