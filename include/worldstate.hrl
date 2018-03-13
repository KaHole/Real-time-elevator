
-record(elevator, {
    behaviour = idle ::atom(), % idle, moving, doorOpen
    floor :: non_neg_integer(),
    direction = stop ::atom(), % stop, up, down
    cabRequests = [] ::[boolean()] 
}).

-record(hallRequest, {
    state = nothing ::atom(), % no_call, new, accepted
    observedBy = [] ::[atom()]
}).

-record(worldState, {
    hallRequests = [] ::[{#hallRequest{}, #hallRequest{}}],
    elevators = [] ::[atom()|#elevator{}]
}).

% lokal state meldingsformat
-record(stateMsg, {
    hallRequests = [] ::[{#hallRequest{}, #hallRequest{}}],
    elevator :: atom()|#elevator{}
}).
