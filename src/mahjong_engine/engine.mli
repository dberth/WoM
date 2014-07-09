(*Copyright (C) 2014 Denis Berthod*)

open Fsm

type player = int

type tile_pos = int (*A position in the initial array*)

type event =
  | Init of Tileset.tile array option
  | Wall_breaker_roll of int
  | Break_wall_roll of int
  | Deal
  | Draw of player * tile_pos
  | Discard of player * tile_pos
  | Mahjong of player
  | Concealed_kong of player * tile_pos
  | Small_kong of player * tile_pos
  | Chow of player * tile_pos list
  | Pong of player * tile_pos list
  | Kong of player * tile_pos list
  | No_action of player

exception Irrelevant_event of (event * string)

val build_engine:
    ?on_game_start_exit: (event, 'world) action ->
      ?on_wait_for_wall_breaker_roll_entry: (event, 'world) action ->
	unit -> ('world -> event list -> 'world * (event, 'world) Fsm.state)
