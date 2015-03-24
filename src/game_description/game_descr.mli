(*Copyright (C) 2015 Denis Berthod*)

type round_player = int

type tile_pos = int

type round_event =
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

type player_idx = int

type ai_conf =
  {
    name: string;
    force: int;
  }

type player_kind =
  | Human
  | AI of ai_conf

type player_descr =
  {
    name: string;
    kind: player_kind;
  }

type rule_descr =
  {
    name: string;
    flags: string list option;
  }

type game_event =
  | Set_rule of rule_descr
  | Player of player_descr
  | East_seat of player_idx
  | Init_score of int
  | Score of (player_idx * int)
  | End_game
  
type game =
  {
    game_events: game_event list;
    current_round: round_event list;
  }

val dump: ?json: bool -> game -> string -> unit

val restore: string -> game
