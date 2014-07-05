(*Copyright (C) 2014 Denis Berthod*)

type 'event state_handlers

type 'event state

val new_state: ('event -> 'event state Lazy.t) -> 'event state

val run: 'event state_handlers -> 'event state Lazy.t -> 'event list -> 'event state

val empty_state_handlers: 'event state_handlers

val set_on_entry: 'event state Lazy.t -> ('event -> unit) -> 'event state_handlers -> 'event state_handlers

