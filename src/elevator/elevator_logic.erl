-module(elevator_logic).
-include("../../include/worldstate.hrl").

-export([start/0]).

start() ->
    {_, Pid} = elevator_interface:start(),
    io:fwrite("~p~n", [Pid]),
    init(Pid, #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}),
    
    % Cab_server_pid = spawn(fun() -> cab_server(
    %         Pid, 
    %         #elevator{floor=elevator_interface:get_floor_sensor_state(Pid)}) 
    %     end),
    % register(cab_server, Cab_server_pid),
    % % io:fwrite("~p~n", [Cab_server_pid]),

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
        between_floor -> State#elevator.floor;
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
        {updated_state, State} -> 
            % Figure out which direction to go
            _State = elevator_algorithm(State),

            % Check if arrive at a wanted floor
            Checked_arrival_state = check_arrival(Pid, _State),

            elevator_interface:set_motor_direction(Pid, Checked_arrival_state#elevator.direction),
            elevator_interface:set_floor_indicator(Pid, Checked_arrival_state#elevator.floor),
            elevator_state_poller ! {self(), Checked_arrival_state}
    end,
    elevator_controller(Pid).


elevator_algorithm(State) ->
    {Cab_request_down, Cab_request_up} = lists:split(State#elevator.floor, State#elevator.cabRequests),

    Go_up = lists:any(fun(X) -> X end, 
        [lists:nth(State#elevator.floor, State#elevator.cabRequests)] ++ Cab_request_up
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

check_arrival(Pid, State) ->
    StopAtFloor = lists:nth(
        State#elevator.floor,
        State#elevator.cabRequests
    ),

    % If we stop return new state, else return old
    case StopAtFloor of
        true -> stop_at_floor(Pid, State);
        _ -> State
    end.

stop_at_floor(Pid, State) ->
    elevator_interface:set_motor_direction(Pid, stop),
    elevator_interface:set_order_button_light(Pid, cab, State#elevator.floor, off),
    elevator_interface:set_door_open_light(Pid, on),
    timer:sleep(2000),  % Remain open for 2 sec. Alt. move to case beneath.
    case elevator_interface:get_obstruction_switch_state(Pid) of
        1 -> stop_at_floor(Pid, State);
        _ -> ok
    end,
    elevator_interface:set_door_open_light(Pid, off), % Close within 5 seconds?
    State#elevator{
        cabRequests=setnth(
            State#elevator.floor,
            State#elevator.cabRequests,
            false
        )
    }.

% https://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].


% % cab_server(Pid, State) ->
% %     receive
% %         {cab_call, Floor} -> cab_server(Pid, cab_call(Pid, State, Floor));
% %         {cab_call_list, Floors} ->
% %         % {hall_call, List} -> ;
% %         _ -> io:format("njet~n"),
% %                 cab_server(Pid, State)
% %     end.

% cab_call(Pid, State, Floor) ->
%     elevator_interface:set_door_open_light(Pid, off),
%     elevator_interface:set_order_button_light(Pid, cab, Floor, on),
%     elevator_interface:set_floor_indicator(Pid, State#elevator.floor),
    
%     ToFloor = State#elevator.floor - Floor,
%     Direction = if 
%         ToFloor > 0 -> down;
%         ToFloor < 0 -> up;
%         true -> stop
%     end,
%     move(Pid, 
%         State#elevator{behaviour=idle, direction=Direction},
%         Floor).

% move(Pid, #elevator{direction=stop} = State, NewFloor) ->
%     elevator_interface:set_motor_direction(Pid, State#elevator.direction),
%     elevator_interface:set_door_open_light(Pid, on),
%     elevator_interface:set_order_button_light(Pid, cab, State#elevator.floor, off),
%     elevator_interface:set_floor_indicator(Pid, State#elevator.floor),
%     State;

% move(Pid, #elevator{behaviour=moving} = State, NewFloor) ->
%     Floor = elevator_interface:get_floor_sensor_state(Pid),
%     timer:sleep(100),
%     elevator_interface:set_floor_indicator(Pid, State#elevator.floor),
%     if 
%         Floor == NewFloor -> move(Pid, 
%                 State#elevator{behaviour=idle, direction=stop, floor=Floor}, NewFloor);
%         Floor /= between_floors -> move(Pid, State#elevator{floor=Floor}, NewFloor);
%         true -> move(Pid, State, NewFloor)
%     end;

% move(Pid, State, NewFloor) ->
%     elevator_interface:set_motor_direction(Pid, State#elevator.direction),
%     timer:sleep(500),
%     move(Pid, State#elevator{behaviour=moving}, NewFloor).
    


    
