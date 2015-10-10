(*Copyright (C) 2015 Denis Berthod*)

type player = int

class rack: string ->
  object
    inherit LTerm_widget.t

    method set_hand: player -> Tileset.tile option list -> unit

    method set_discard: player -> Tileset.tile option list -> unit

    method set_exposed: player -> Tileset.tile option list list -> unit 

    method set_name: player -> string -> unit

    method set_seat_wind: player -> Common.wind -> unit

    method set_reverse_mode: bool -> unit

    method width: LTerm_draw.context -> int option

    method reverse_mode: bool

    method clear_selection: unit

    method select_prev_event: unit

    method select_next_event: unit

    method selected_event: Game_descr.round_event option

    method set_selected_tile: Tileset.tile  -> unit

    method set_events:  (Game_descr.round_event * Tileset.tile list) list -> unit
  end
