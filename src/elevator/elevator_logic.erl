-module(elevator_logic).
-include("../../include/worldstate.hrl").

-export([start/1]).

start(Pid) ->
    % {_, Pid} = elevator_interface:start(),
    io:fwrite("~p~n", [Pid]),
    init(Pid, #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}),

    Dummy_state = #elevator{
        behaviour=idle,
        floor=elevator_interface:get_floor_sensor_state(Pid),
        direction=stop,
        cabRequests=lists:duplicate(4, false)
    },
    register(elevator_state_poller,
        spawn(fun() -> 
            elevator_state_poller(
                Pid,
                Dummy_state
            ) 
            end
        )
    ),
    register(elevator_controller, 
        spawn(fun() -> 
            elevator_controller(
                Pid
            )
            end
        )
    ).


init(Pid, #elevator{floor=between_floors}) ->
    elevator_interface:set_motor_direction(Pid, up),
    timer:sleep(500),
    Floor = elevator_interface:get_floor_sensor_state(Pid),
    init(Pid, #elevator{floor=Floor});

init(Pid, _) -> 
    elevator_interface:set_motor_direction(Pid, stop),
    ok.

elevator_state_poller(Pid, State) ->
    % Polles new state and merges with existing
    Polled_panel_state = get_floor_panel_state(Pid, [], length(State#elevator.cabRequests)-1),

    % Get floor number. Ignores between_floor
    AtFloor = case elevator_interface:get_floor_sensor_state(Pid) of
        between_floors -> State#elevator.floor;
        _ -> elevator_interface:get_floor_sensor_state(Pid)
    end,

    _State = State#elevator{
        floor=AtFloor,
        cabRequests=[A or B || {A,B} <- lists:zip(State#elevator.cabRequests, Polled_panel_state)]
    },

    receive
        {Sender, get_state} -> 
            Sender ! {updated_state, _State};
        {_, NewState} ->
            elevator_state_poller(Pid, NewState#elevator{
                floor=AtFloor,
                cabRequests=[A or B || {A,B} <- lists:zip(NewState#elevator.cabRequests, Polled_panel_state)]
            })
    end,
    elevator_state_poller(Pid, _State).

get_floor_panel_state(Pid, Floor_list, 0) ->
    Floor_state = elevator_interface:get_order_button_state(Pid, 0, cab),
    [A == 1 || A <- lists:append([Floor_state], Floor_list)];

get_floor_panel_state(Pid, Floor_list, Floor_number) -> 
    Floor_state = elevator_interface:get_order_button_state(Pid, Floor_number, cab),
    get_floor_panel_state(Pid, lists:append([Floor_state], Floor_list), Floor_number-1).

elevator_controller(Pid) -> 
    % Checks for pressed cab floor panel buttons
    % Polled_panel_state = get_floor_panel_state(Pid, [], length(State#elevator.cabRequests)),
    elevator_state_poller ! {self(), get_state},
    receive
        {updated_state, {State, HallCalls}} -> 
            % Handle hall calls as cab calls temporarily for determining direction of elevator
            CabHallCall = [ Cab or Up or Down || {Cab,{Up,Down}} <- lists:zip(State#elevator.cabRequests, HallCalls)]
            
            % Figure out which direction to go
            _State = elevator_algorithm(State, CabHallCall),

            % Check if arrive at a wanted floor
            Checked_arrival_state = check_arrival(Pid, _State, CabHallCall, HallCalls),

            elevator_interface:set_motor_direction(Pid, Checked_arrival_state#elevator.direction),
            elevator_interface:set_floor_indicator(Pid, Checked_arrival_state#elevator.floor),
            elevator_state_poller ! {self(), Checked_arrival_state}
    end,
    elevator_controller(Pid).

elevator_algorithm(State, CabHallCall) ->
    % {Cab_request_down, Cab_request_up} = lists:split(State#elevator.floor+1, State#elevator.cabRequests),

    % Go_up = lists:any(fun(X) -> X end, 
    %     [lists:nth(State#elevator.floor+1, State#elevator.cabRequests)] ++ Cab_request_up
    % ),

    {Cab_request_down, Cab_request_up} = lists:split(State#elevator.floor+1, CabHallCall),

    Go_up = lists:any(fun(X) -> X end, 
        [lists:nth(State#elevator.floor+1, CabHallCall)] ++ Cab_request_up
    ),

    Go_down = lists:any(fun(X) -> X end, Cab_request_down),
    Continue = case State#elevator.direction of
        up -> Go_up;
        down -> Go_down;
        stop -> false
    end,

    case Continue of 
        false -> 
        if
            Go_up ->
                State#elevator{
                    direction=up
                }
            ;
            Go_down ->
                State#elevator{
                    direction=down
                }
            ;
            true -> 
                State#elevator{
                    behaviour=idle,
                    direction=stop,
                    cabRequests=lists:duplicate(length(State#elevator.cabRequests), false)
                }
        end;
        _ -> State
    end.

check_arrival(Pid, State, CabHallCall, HallCalls) ->
    % StopAtFloor = lists:nth(
    %     State#elevator.floor+1,
    %     State#elevator.cabRequests
    % ),

    StopAtFloor = lists:nth(
        State#elevator.floor+1,
        CabHallCall
    ),

    % If we stop return new state, else return old
    case StopAtFloor of
        true -> stop_at_floor(Pid, State, HallCalls);
        _ -> State
    end.

stop_at_floor(Pid, State, HallCalls) ->
    elevator_interface:set_motor_direction(Pid, stop),
    elevator_interface:set_order_button_light(Pid, cab, State#elevator.floor, off),
    elevator_interface:set_door_open_light(Pid, on),
    timer:sleep(2000),  % Remain open for 2 sec. Alt. move to case beneath.
    case elevator_interface:get_obstruction_switch_state(Pid) of
        1 -> stop_at_floor(Pid, State);
        _ -> ok
    end,
    elevator_interface:set_door_open_light(Pid, off), % Close within 5 seconds?
    
    _State = State#elevator{
        cabRequests=setnth(
            State#elevator.floor+1,
            State#elevator.cabRequests,
            done
        )
    },
    {Up, Down} = lists:nth(
        State#elevator.floor+1,
        HallCalls
    ),
    _HallCalls = if 
        Up and State#elevator.direction == up -> setnth(
            State#elevator.floor+1,
            HallCalls,
            {done, Down}
        );
        Down and State#elevator.direction == down -> setnth(
            State#elevator.floor+1,
            HallCalls,
            {Up, done}
        );
        _ -> ok
    end,
    {_State, _HallCalls}.

% https://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].