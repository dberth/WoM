(*Copyright (C) 2014 Denis Berthod*)

type player = int

type tile_pos = int (*A position in the initial array*)

type event =
  | Init of Tileset.tile array
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

exception Irrelevent_event of (event * string)

val start_engine: (event, 'world) Fsm.action_handler -> 'world -> event list -> 'world * (event, 'world) Fsm.state
