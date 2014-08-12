(*Copyright (C) 2014 Denis Berthod*)

(*Finite State Machine*)

module IMap = Map.Make(struct type t = int let compare = compare end)

type ('event, 'world) action = 'event -> 'world -> 'world

type ('event, 'world) action_handler = ('event, 'world) action IMap.t

type ('event, 'world) setter = ('event, 'world) action -> ('event, 'world) action_handler -> ('event, 'world) action_handler

type ('event, 'world) state =
  {
    on_entry_handle: int;
    set_on_entry: ('event, 'world) setter;
    set_on_exit: ('event, 'world) setter;
    state: (('event, 'world) action_handler -> 'world -> 'event list -> 'world * ('event, 'world) state);
    accepted_events: ('world -> 'event list);
    history: 'event list;
  }

let empty_action_handler = IMap.empty

let new_id =
  let id = ref (-1) in
  fun () -> incr id; !id

let run ?(with_history = false) action_handler world state events =
  let new_world, new_state = (Lazy.force state).state action_handler world events in
  let new_state =
    if with_history then
      {new_state with history = List.rev_append events (Lazy.force state).history}
    else
      new_state
  in
  new_world, new_state

let on_entry state event world =
  (Lazy.force state).set_on_entry event world

let on_exit state event world =
  (Lazy.force state).set_on_exit event world

let new_state ?(accepted_events = (fun _ -> [])) transition =
  let on_entry_handle = new_id () in
  let exit_id = new_id () in
  let set_on_entry action action_handler = IMap.add on_entry_handle action action_handler in
  let set_on_exit action action_handler = IMap.add exit_id action action_handler in
  let on_event id action_handler event world =
    begin match IMap.find id action_handler with
    | action -> action event world
    | exception Not_found -> world
    end
  in
  let history = [] in
  let on_exit = on_event exit_id in
  let rec state action_handler world = function
    | [] -> world, {on_entry_handle; set_on_entry; set_on_exit; state; accepted_events; history}
    | event :: tl ->
      let next_state = transition event in
      let world = on_exit action_handler event world in
      let world = on_event (Lazy.force next_state).on_entry_handle action_handler event world in
      run action_handler world next_state tl
  in
  {on_entry_handle; set_on_entry; set_on_exit; state; accepted_events; history}

let accepted_events world {accepted_events; _} = accepted_events world
