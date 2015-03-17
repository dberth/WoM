(*Copyright (C) 2014 Denis Berthod*)

type game

type declared = (Tileset.tileset * Game_descr.tile_pos list * bool (*is_concealed*)) list

type extraordinary_event =
  | Final_draw
  | Final_discard
  | Win_on_kong
  | Kong_robbing
  | Blessing_of_heaven
  | Blessing_of_earth

type mahjong =
  {
    declared: declared;
    hand: Tileset.tileset;
    discard_player: Game_descr.round_player option;
    last_drawn_tile: Tileset.tile option;
    extraordinary_events: extraordinary_event list;
  }

type end_game =
  | No_winner
  | Mahjong of mahjong


exception Irrelevant_event of (Game_descr.event * string)

val build_engine:
  seven_pairs: bool ->
  ?irregular_hands: Tileset.irregular_hands ->
  Game_descr.event list ->
  (Game_descr.event, game) Fsm.action_handler * game * (Game_descr.event, game) Fsm.state

val finished: game -> end_game option

val string_of_end_game: end_game -> string

val string_of_game: game -> string

val string_of_event: game -> Game_descr.event -> string

val known_tiles: game -> Tileset.tile option array

val random_game: Tileset.tile option array

val current_player: game -> Game_descr.round_player

val current_player_hand: game -> Tileset.tileset

val player_hand: Game_descr.round_player -> game -> Tileset.tileset

val player_discarded_tiles: Game_descr.round_player -> game -> Tileset.tile list

val player_declared_sets: Game_descr.round_player -> game -> declared

val nb_tiles_in_hand: Game_descr.round_player -> game -> int

val tile_of_tile_pos: game -> int -> Tileset.tile

val descr_of_tile_pos: game -> int -> Tileset.tile_descr

val is_in_current_player_hand: game -> int -> bool

val discarded_tile: game -> Tileset.tile option

val discard_player: game -> int option

val current_player_wind: game -> Tileset.tile

val last_drawn_tile: Game_descr.round_player -> game -> Tileset.tile option

val set_real_init_tiles: Game_descr.event list -> game -> Game_descr.event list
