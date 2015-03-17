(*Copyright (C) 2015 Denis Berthod*)

type round_player = int

type tile_pos = int

type event =
  | Init of Tileset.tile option array
  | Wall_breaker_roll of int
  | Break_wall_roll of int
  | Deal
  | Draw of round_player
  | Discard of (round_player * tile_pos)
  | Mahjong of round_player
  | Concealed_kong of (round_player * tile_pos list)
  | Small_kong of (round_player * tile_pos)
  | Chow of (round_player * tile_pos list)
  | Pong of (round_player * tile_pos list)
  | Kong of (round_player * tile_pos list)
  | No_action of round_player

type game =
  {
    current_round: event list;
  }

val dump: ?json: bool -> game -> string -> unit

val restore: string -> game
