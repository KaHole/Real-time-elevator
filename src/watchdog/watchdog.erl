-module(watchdog).

-export([start/1]).

-define(TIMEOUT, 25).
-define(QUARANTINE, 10000).

start(HallRequestState) -> 
    register(watchdog, 
        spawn(fun() -> watchdog(HallRequestState, lists:duplicate(length(HallRequestState)*2, 0), []) end)
    ).

watchdog(HallRequestStates, HallRequestTimes, AssignedElevators) ->
    receive
        {watch_hall_requests, _HallRequestStates, _AssignedElevators} ->
            Diff = [(New == Old) and (New == accepted) || {New, Old} <- lists:zip(lists:flatten(_HallRequestStates), lists:flatten(HallRequestStates))],
            _HallRequestTimes = update_times(Diff, HallRequestTimes),
            watchdog(_HallRequestStates, _HallRequestTimes, _AssignedElevators);
        {kill} ->
            io:fwrite("Timed out, got killed. ~p~n.", [self()]),
            exit(whereis(discover), kill),
            stop_elevator(),
            net_kernel:stop(),
            timer:sleep(?QUARANTINE),
            io:fwrite("Karantene~n"),
            init:restart(),
            ok
        after 1000 -> ok
    end,
    io:fwrite("~p~n", [get_time()]),
    HallRequestTimeouts = list_to_tuples(timed_out(HallRequestTimes)),
    ResetTimeouts = reset_timeouts(HallRequestTimes),
    TimedOutElevators = lists:delete(node(), find_timed_out_elevators(HallRequestTimeouts, AssignedElevators)),
    [{watchdog, N} ! {kill} || N <- TimedOutElevators],
    watchdog(HallRequestStates, ResetTimeouts, AssignedElevators).

update_times([], []) -> [];
update_times([true|StateTail], [TimeHead|TimeTail]) -> [TimeHead|update_times(StateTail, TimeTail)];
update_times([false|StateTail], [_|TimeTail]) -> [get_time()|update_times(StateTail, TimeTail)].

timed_out([]) -> [];
timed_out([TimeHead|TimeTail]) -> [TimeHead+?TIMEOUT < get_time()|timed_out(TimeTail)].

reset_timeouts([]) -> [];
reset_timeouts([TimeHead|TimeTail]) -> 
    Time = get_time(),
    Reset = if
        TimeHead+?TIMEOUT < Time -> Time;
        true -> TimeHead
    end,
    [Reset|reset_timeouts(TimeTail)].

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

stop_elevator() ->
    exit(whereis(elevator_controller), kill),
    elevator_interface:set_motor_direction(whereis(driver_pid), stop).