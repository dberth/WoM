(*Copyright (C) 2014 Denis Berthod*)

type ('event, 'world) actuator = 'event -> 'world -> 'world

type ('event, 'world)  actuators

type ('event, 'world) state

val new_state: ('event -> ('event, 'world) state Lazy.t) -> ('event, 'world) state

val run: ('event, 'world) actuators -> 'world -> ('event, 'world) state Lazy.t -> 'event list -> 'world * ('event, 'world) state

val empty_actuators: ('event, 'world) actuators

val set_on_entry: ('event, 'world) state Lazy.t -> ('event, 'world) actuator -> ('event, 'world) actuators -> ('event, 'world) actuators

