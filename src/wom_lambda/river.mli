(*Copyright (C) 2015 Denis Berthod*)

class river: int -> string ->
  object
    inherit LTerm_widget.t

    method width: int

    method height: int

    method set_wall_start: int -> unit

    method set_nb_tiles_in_kong_box: int -> unit

    method set_last_tile: int -> unit

    method set_die_1: int option -> unit

    method set_die_2: int option -> unit

    method set_seat_wind: int (*player*) -> Common.wind -> unit

    method set_tile: (int * Tileset.tile) option -> unit

    method set_prevailing_wind: Common.wind option -> unit

  end
