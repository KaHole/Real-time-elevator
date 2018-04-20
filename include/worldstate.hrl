
-record(elevator, {
    behaviour = idle ::atom(), % idle, moving, doorOpen
    floor = 0 ::non_neg_integer(),
    direction = stop ::atom(), % stop, up, down
    cabCalls = [] ::[boolean()],
    betweenFloors = false ::boolean()
}).

-record(hallRequest, {
    state = unknown ::atom(), % unknown, nothing, new, accepted, done
    observedBy = [] ::[atom()]
}).