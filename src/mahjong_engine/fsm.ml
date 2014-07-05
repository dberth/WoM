(*Copyright (C) 2014 Denis Berthod*)

(*Finite State Machine*)

module IMap = Map.Make(struct type t = int let compare = compare end)

type ('event, 'world) actuator = 'event -> 'world -> 'world

type ('event, 'world) actuators = ('event, 'world) actuator IMap.t

type ('event, 'world) state =
    {
     set_on_entry: (('event, 'world) actuator -> ('event, 'world) actuators -> ('event, 'world) actuators);
     state: (('event, 'world) actuators -> 'world -> 'event list -> 'world * ('event, 'world) state)
   }

let empty_actuators = IMap.empty

let new_id =
  let id = ref (-1) in
  fun () -> incr id; !id

let run actuators world state events =
  (Lazy.force state).state actuators world events

let set_on_entry state =
  (Lazy.force state).set_on_entry

let new_state transition =
  let id = new_id () in
  let set_on_entry actuator actuators = IMap.add id actuator actuators in
  let on_entry actuators event world =
    match IMap.find id actuators with
    | actuator -> actuator event world
    | exception Not_found -> world
  in
  let rec state actuators world = function
    | [] -> world, {set_on_entry; state}
    | event :: tl ->
	let world = on_entry actuators event world in
	run actuators world (transition event) tl
  in
  {set_on_entry; state}

(******* put in tests *****)



let string_of_light = function
  | `RED -> "RED"
  | `GREEN -> "GREEN"
  | `ORANGE -> "ORANGE"

let rec red = lazy (new_state (function `GREEN -> green | _ -> assert false))

and green = lazy (new_state (function `ORANGE -> orange | _ -> assert false))

and orange = lazy(new_state (function `RED -> red | _ -> assert false))

let state_handlers =
  let f event _ = event in
  empty_actuators |>
  set_on_entry red f |>
  set_on_entry green f |>
  set_on_entry orange f

let _ = run state_handlers `RED red [`GREEN; `ORANGE; `RED; `GREEN]
