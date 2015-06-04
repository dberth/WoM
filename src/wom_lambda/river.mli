(*Copyright (C) 2015 Denis Berthod*)

class river: int -> string ->
  object
    inherit LTerm_widget.t

    method width: int

    method set_wall_start: int -> unit

    method set_nb_tiles_in_kong_box: int -> unit

    method set_last_tile: int -> unit

    method set_die_1: int option -> unit

    method set_die_2: int option -> unit
  end
