(*Copyright (C) 2014 Denis Berthod*)

type ('event, 'world) action = 'event -> 'world -> 'world

type ('event, 'world)  action_handler

type ('event, 'world) state

val new_state: ('event -> ('event, 'world) state Lazy.t) -> ('event, 'world) state

val run: ('event, 'world) action_handler -> 'world -> ('event, 'world) state Lazy.t -> 'event list -> 'world * ('event, 'world) state

val empty_action_handler: ('event, 'world) action_handler

val on_entry:  ('event, 'world) state Lazy.t -> ('event, 'world) action -> ('event, 'world) action_handler -> ('event, 'world) action_handler

val on_exit: ('event, 'world) state Lazy.t -> ('event, 'world) action -> ('event, 'world) action_handler -> ('event, 'world) action_handler

