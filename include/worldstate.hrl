
-record(elevator, {
    behaviour = idle ::atom(), % idle, moving, doorOpen
    floor = 0 ::non_neg_integer(),
    direction = stop ::atom(), % stop, up, down
    cabRequests = [] ::[boolean()] 
}).

-record(hallRequest, {
    state = nothing ::atom(), % nothing, new, accepted, done
    observedBy = [] ::[atom()]
}).

% Obsolete?
% we should still enforce atom()|elevator tuple somehow? or not?
% -record(worldState, {
%     hallRequests = [] ::[{#hallRequest{}, #hallRequest{}}], % list of tuples of up/down hall-request pairs
%     elevators = [] ::[atom()|#elevator{}] % tuple of node-id and elevator object
% }).