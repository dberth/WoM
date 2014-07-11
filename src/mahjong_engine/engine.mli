(*Copyright (C) 2014 Denis Berthod*)

type player = int

type tile_pos = int (*A position in the initial array*)

type event =
  | Init of Tileset.tile_descr option array
  | Wall_breaker_roll of int
  | Break_wall_roll of int
  | Deal
  | Draw of player
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
  ?on_wait_for_wall_breaker_roll_entry: (event, 'world) Fsm.action ->
  ?on_wait_for_break_roll_entry: (event, 'world) Fsm.action ->
  ?on_wait_for_deal_entry: (event, 'world) Fsm.action ->
  ?on_wait_for_draw_in_wall_entry: (event, 'world) Fsm.action ->
  ?on_player_turn_entry: (event, 'world) Fsm.action ->
  ?on_tile_discarded_entry: (event, 'world) Fsm.action ->
  ?on_td_1_no_action_2_entry: (event, 'world) Fsm.action ->
  ?on_td_1_chow_2_entry: (event, 'world) Fsm.action ->
  ?on_td_1_pong_2_entry: (event, 'world) Fsm.action ->
  ?on_td_1_kong_2_entry: (event, 'world) Fsm.action ->
  ?on_td_1_no_action_1_entry: (event, 'world) Fsm.action ->
  ?on_td_2_pong_1_entry: (event, 'world) Fsm.action ->
  ?on_td_2_kong_1_entry: (event, 'world) Fsm.action ->
  ?on_td_1_chow_1_entry: (event, 'world) Fsm.action ->
  ?on_td_1_pong_1_entry: (event, 'world) Fsm.action ->
  ?on_td_1_kong_1_entry: (event, 'world) Fsm.action ->
  ?on_mahjong_declared_entry: (event, 'world) Fsm.action ->
  ?on_kong_declared_entry: (event, 'world) Fsm.action ->
  ?on_wait_for_kong_robbing_entry: (event, 'world) Fsm.action ->
  ?on_kr_2_entry: (event, 'world) Fsm.action ->
  ?on_kr_1_entry: (event, 'world) Fsm.action ->

  ?on_game_start_exit: (event, 'world) Fsm.action ->
  ?on_wait_for_wall_breaker_roll_exit: (event, 'world) Fsm.action ->
  ?on_wait_for_break_roll_exit: (event, 'world) Fsm.action ->
  ?on_wait_for_deal_exit: (event, 'world) Fsm.action ->
  ?on_wait_for_draw_in_wall_exit: (event, 'world) Fsm.action ->
  ?on_player_turn_exit: (event, 'world) Fsm.action ->
  ?on_tile_discarded_exit: (event, 'world) Fsm.action ->
  ?on_td_1_no_action_2_exit: (event, 'world) Fsm.action ->
  ?on_td_1_chow_2_exit: (event, 'world) Fsm.action ->
  ?on_td_1_pong_2_exit: (event, 'world) Fsm.action ->
  ?on_td_1_kong_2_exit: (event, 'world) Fsm.action ->
  ?on_td_1_no_action_1_exit: (event, 'world) Fsm.action ->
  ?on_td_2_pong_1_exit: (event, 'world) Fsm.action ->
  ?on_td_2_kong_1_exit: (event, 'world) Fsm.action ->
  ?on_td_1_chow_1_exit: (event, 'world) Fsm.action ->
  ?on_td_1_pong_1_exit: (event, 'world) Fsm.action ->
  ?on_td_1_kong_1_exit: (event, 'world) Fsm.action ->
  ?on_mahjong_declared_exit: (event, 'world) Fsm.action ->
  ?on_kong_declared_exit: (event, 'world) Fsm.action ->
  ?on_wait_for_kong_robbing_exit: (event, 'world) Fsm.action ->
  ?on_kr_2_exit: (event, 'world) Fsm.action ->
  ?on_kr_1_exit: (event, 'world) Fsm.action ->

  unit -> ('world -> event list -> 'world * (event, 'world) Fsm.state)
