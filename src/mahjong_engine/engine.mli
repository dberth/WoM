(*Copyright (C) 2014 Denis Berthod*)

type round

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

type end_round =
  | No_winner
  | Mahjong of mahjong


exception Irrelevant_event of (Game_descr.round_event * string)

val build_engine:
  seven_pairs: bool ->
  ?irregular_hands: Tileset.irregular_hands ->
  Game_descr.round_event list ->
  (Game_descr.round_event, round) Fsm.action_handler * round * (Game_descr.round_event, round) Fsm.state

val finished: round -> end_round option

val string_of_end_round: end_round -> string

val string_of_round: round -> string

val string_of_event: round -> Game_descr.round_event -> string

val known_tiles: round -> Tileset.tile option array

val random_game: Tileset.tile option array

val current_player: round -> Game_descr.round_player

val current_player_hand: round -> Tileset.tileset

val player_hand: Game_descr.round_player -> round -> Tileset.tileset

val player_discarded_tiles: Game_descr.round_player -> round -> Tileset.tile list

val player_declared_sets: Game_descr.round_player -> round -> declared

val nb_tiles_in_hand: Game_descr.round_player -> round -> int

val tile_of_tile_pos: round -> int -> Tileset.tile

val descr_of_tile_pos: round -> int -> Tileset.tile_descr

val is_in_current_player_hand: round -> int -> bool

val discarded_tile: round -> Tileset.tile option

val discard_player: round -> int option

val current_player_wind: round -> Tileset.tile

val last_drawn_tile: Game_descr.round_player -> round -> Tileset.tile option

val set_real_init_tiles: Game_descr.round_event list -> round -> Game_descr.round_event list
