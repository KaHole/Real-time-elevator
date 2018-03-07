
-record(elevator, {
    id,
    behaviour = idle ::atom(), % idle, moving, doorOpen
    floor :: non_neg_integer(),
    direction = stop ::atom(), % stop, up, down
    cabRequests = [] ::[boolean()] 
}).

-record(hallRequest, {
    state = new ::atom(), % new, pending, done
    observedBy = [] ::atom()
}).

% -record(hall, {
%     up ::#hallRequest{},
%     down ::#hallRequest{},
% }).

-record(worldState, {
    hallRequests = [] ::[{#hallRequest{}, #hallRequest{}}],
    elevators = [] ::[#elevator{}]
}).

% lokal state meldingsformat
-record(stateMsg, {
    hallRequests = [] ::[#hallRequest{}],
    elevator ::#elevator{}
}).
