(*Copyright (C) 2014 Denis Berthod*)

(*Finite State Machine*)

module IMap = Map.Make(struct type t = int let compare = compare end)

type 'event state_handlers = ('event -> unit) IMap.t

type 'event state =
    {
     set_on_entry: (('event -> unit) -> 'event state_handlers -> 'event state_handlers);
     state: ('event state_handlers -> 'event list -> 'event state)
   }

let empty_state_handlers = IMap.empty

let new_id =
  let id = ref (-1) in
  fun () -> incr id; !id

let run state_handlers state events =
  (Lazy.force state).state state_handlers events

let set_on_entry state state_handlers =
  (Lazy.force state).set_on_entry state_handlers

let new_state (transition: 'event -> 'event state Lazy.t) =
  let id = new_id () in
  let set_on_entry handler state_handlers = IMap.add id handler state_handlers in
  let on_entry state_handlers event =
    match IMap.find id state_handlers with
    | handler -> handler event
    | exception Not_found -> ()
  in
  let rec state state_handlers = function
    | [] -> {set_on_entry; state}
    | event :: tl ->
	on_entry state_handlers event;
	run state_handlers (transition event) tl
  in 	  
  {set_on_entry; state}

(******* put in tests *****)

let light = ref `RED

let string_of_light = function
  | `RED -> "RED"
  | `GREEN -> "GREEN"
  | `ORANGE -> "ORANGE"

let rec red = lazy (new_state (function `GREEN -> green | _ -> assert false))

and green = lazy (new_state (function `ORANGE -> orange | _ -> assert false))

and orange = lazy(new_state (function `RED -> red | _ -> assert false))

let state_handlers =
  let f event = print_endline (string_of_light event); light := event in
  empty_state_handlers |>
  set_on_entry red f |>
  set_on_entry green f |>
  set_on_entry orange f

let _ = run state_handlers red [`GREEN; `ORANGE; `RED; `GREEN]
