-module(state_poller).

-export([start/1]).

start(Pid) ->
    register(state_poller, spawn(fun() -> poll_elevator_state(Pid, Elevator) end)).

poll_elevator_state(Pid, State) ->
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
