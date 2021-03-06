(*Copyright (C) 2014 Denis Berthod*)

(** {2 States} *)

type ('event, 'world) state
  (** Type a state acception events of type ['event] and modifying the
      world of type ['world].
  *)


val new_state:
  ?accepted_events: ('world -> 'event list) ->
  ('event -> ('event, 'world) state Lazy.t) ->
  ('event, 'world) state

(** {2 actions} *)

type ('event, 'world) action = 'event -> 'world -> 'world
  (**
     Describe how an event modify the world either on state entry
     or on state exit.
  *)

type ('event, 'world)  action_handler
  (**
     A set on action registered on the entry or the exit of states.
  *)

val empty_action_handler: ('event, 'world) action_handler
  (**
     An empty set of action.
  *)

val on_entry:  ('event, 'world) state Lazy.t -> ('event, 'world) action -> ('event, 'world) action_handler -> ('event, 'world) action_handler
  (**
     [on_entry state action action_handler] returns a [action_handler]
     with [action] sets on the entry of the state [state].
  *)

val on_exit: ('event, 'world) state Lazy.t -> ('event, 'world) action -> ('event, 'world) action_handler -> ('event, 'world) action_handler
  (**
     [on_exit state action action_handler] returns a [action_handler]
     with [action] sets on the exit of the state [state].
  *)

val add_exit_state_hook: ('event -> 'world -> unit) -> ('event, 'world) action_handler -> ('event, 'world) action_handler
    (**
       [add_exit_state_hook hook action_handler] returns an action handler
       where [hook] is called each time a state is exited.
       The event is the event is the one that trigger the state exit.
       The world is the one resulting of the application
       of the exit actions of the state.
       Several hooks can be set on the same action handler.
       The order of execution is not specified.
    *)

(** {2 Runner} *)

val run: ?with_history: bool -> ?history_rewriters: ('world -> 'event -> 'event) list -> ('event, 'world) action_handler -> 'world -> ('event, 'world) state Lazy.t -> 'event list -> 'world * ('event, 'world) state
    (**
       [run action_handler world state events] runs the [events] starting
       from [state]. Actions defined in [action_handler] are performed
       on each state entry and exit to change the [world].
       Returns the new state of the world and the new state of the machine.
       If with_history is set, an history of the events is kept to allow replay.
       History rewiters allow to change an event before it is commited to history.
    *)

(** {2 Helpers} *)

val accepted_events: 'world -> ('event, 'world) state -> 'event list

(** {2 History handling} *)

val history: ('event, 'world) state -> 'event list
(**
Returns the history of all events ran with the with_history flag. 
*)
    
(** {2 Usage Example} *)

(**
       Red light French style i.e. RED -> GREEN -> ORANGE -> RED -> ...
       Commanded by a timer that produce a `TIMEOUT event.

       let rec red = lazy (new_state (function `TIMEOUT -> green | _ -> red))

       and green = lazy (new_state (function `TIMEOUT -> orange | _ -> green))

       and orange = lazy(new_state (function `TIMEOUT -> red | _ -> orange))

       let state_handlers =
       let entry light_color state =
        fun _event _previous_light_color ->
          print_endline (Printf.sprintf "Enter %s" state); light_color
       in
       let exit state _ world = print_endline (Printf.sprintf "Exit %s" state); world in
       empty_action_handler |>
       on_entry red (entry `RED "RED") |>
       on_entry green (entry `GREEN "GREEN") |>
       on_entry orange (entry `ORANGE "ORANGE") |>
       on_exit red (exit "RED") |>
       on_exit green (exit "GREEN") |>
       on_exit orange (exit "ORANGE")
       

       let _ = run state_handlers `RED red [`TIMEOUT; `TIMEOUT; `TIMEOUT; `TIMEOUT]

*)
