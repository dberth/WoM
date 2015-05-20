(*Copyright (C) 2015 Denis Berthod*)

type player = int

class rack: string ->
  object
    inherit LTerm_widget.t

    method set_hand: player -> Tileset.tile option list -> unit

    method set_discard: player -> Tileset.tile option list -> unit

    method set_exposed: player -> Tileset.tile option list list -> unit 

    method set_name: player -> string -> unit

    method set_seat_wind: player -> string -> unit

    method set_reverse_mode: bool -> unit

    method reverse_mode: bool
  end
