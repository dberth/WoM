(*Copyright (C) 2014 Denis Berthod*)

type player = int

type tile_pos = int (*A position in the initial array*)

type game

type declared = (Tileset.tileset * tile_pos list * bool (*is_concealed*)) list

type mahjong =
  {
    declared: declared;
    hand: Tileset.tileset;
    discard_player: player option;
    kong_robbing: bool
  }

type end_game =
  | No_winner
  | Mahjong of mahjong

type event =
  | Init of Tileset.tile option array
  | Wall_breaker_roll of int
  | Break_wall_roll of int
  | Deal
  | Draw of player
  | Discard of player * tile_pos
  | Mahjong of player
  | Concealed_kong of player * tile_pos list
  | Small_kong of player * tile_pos
  | Chow of player * tile_pos list
  | Pong of player * tile_pos list
  | Kong of player * tile_pos list
  | No_action of player

exception Irrelevant_event of (event * string)

val build_engine:
  ?irregular_hands: Tileset.irregular_hands ->
  event list ->
  (event, game) Fsm.action_handler * game * (event, game) Fsm.state

val finished: game -> end_game option

val string_of_end_game: end_game -> string

val string_of_game: game -> string

val string_of_event: game -> event -> string

val known_tiles: game -> Tileset.tile option array

val random_game: Tileset.tile option array

val current_player: game -> player

val current_player_hand: game -> Tileset.tileset

val player_hand: player -> game -> Tileset.tileset

val player_discarded_tiles: player -> game -> Tileset.tile list

val player_declared_sets: player -> game -> declared

val nb_tiles_in_hand: player -> game -> int

val tile_of_tile_pos: game -> int -> Tileset.tile

val descr_of_tile_pos: game -> int -> Tileset.tile_descr

val is_in_current_player_hand: game -> int -> bool

val discarded_tile: game -> Tileset.tile option

val discard_player: game -> int option
