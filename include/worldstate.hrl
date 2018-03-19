
-record(elevator, {
    behaviour = idle ::atom(), % idle, moving, doorOpen
    floor :: non_neg_integer(),
    direction = stop ::atom(), % stop, up, down
    cabRequests = [] ::[boolean()] 
}).

-record(hallRequest, {
    state = nothing ::atom(), % nothing, new, accepted
    observedBy = [] ::[atom()]
}).

% Obsolete?
% we should still enforce atom()|elevator tuple somehow? shit
% -record(worldState, {
%     hallRequests = [] ::[{#hallRequest{}, #hallRequest{}}], % list of tuples of up/down hall-request pairs
%     elevators = [] ::[atom()|#elevator{}] % tuple of node-id and elevator object
% }).
