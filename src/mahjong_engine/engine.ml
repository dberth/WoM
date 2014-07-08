(*Copyright (C) 2014 Denis Berthod*)

open Tileset
open Fsm

type player = int

type tile_pos = int (*A position in the initial array*)

type event =
  | Init of tile array
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

let start_engine action_handler world events = 
  let rec game_start = lazy (new_state (function
    | Init _ -> wait_for_wall_breaker_roll
    | event -> raise (Irrelevent_event (event, "game_start"))))

  and wait_for_wall_breaker_roll = lazy (new_state (function
    | Wall_breaker_roll x when (1 < x && x <= 12) -> wait_for_break_roll
    | event -> raise (Irrelevent_event (event, "wait_for_wall_break_roll"))))

  and wait_for_break_roll = lazy (new_state (function
    | Break_wall_roll x when (1 < x && x <= 12) -> wait_for_deal
    | event -> raise (Irrelevent_event (event, "wait_for_break_roll"))))

  and wait_for_deal = lazy (new_state (function
    | Deal -> wait_for_draw_in_wall
    | event -> raise (Irrelevent_event (event, "wait_for_deal"))))

  and wait_for_draw_in_wall = lazy (new_state (function
    | Draw _ -> player_turn
    | event -> raise (Irrelevent_event (event, "wait_for_draw_in_wall"))))

  and player_turn = lazy (new_state (function
    | Discard _ -> tile_discarded
    | Mahjong _ -> mahjong_declared
    | Concealed_kong _ -> kong_declared
    | Small_kong _ -> wait_for_kong_robbing
    | event -> raise (Irrelevent_event (event, "player_turn"))))

  and tile_discarded = lazy (new_state (function
    | No_action _ -> td_1_no_action_2
    | Mahjong _ -> mahjong_declared
    | Chow _ -> td_1_chow_2
    | Pong _ -> td_1_pong_2
    | Kong _ -> td_1_kong_2
    | event -> raise (Irrelevent_event (event, "tile_discarded"))))

  and td_1_no_action_2 = lazy (new_state (function
    | No_action _ -> td_1_no_action_1
    | Mahjong _ -> mahjong_declared
    | Pong _ -> td_2_pong_1
    | Kong _ -> td_2_kong_1
    | event -> raise (Irrelevent_event (event, "1_no_action_2"))))

  and td_1_chow_2 = lazy (new_state (function
    | No_action _ -> td_1_chow_1
    | Mahjong _ -> mahjong_declared
    | Pong _ -> td_2_pong_1
    | Kong _ -> td_2_kong_1
    | event -> raise (Irrelevent_event (event, "1_chow_2"))))

  and td_1_pong_2 = lazy (new_state (function
    | No_action _ -> td_1_pong_1
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevent_event (event, "1_pong_2"))))

  and td_1_kong_2 = lazy (new_state (function
    | No_action _ -> td_1_kong_1
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevent_event (event, "1_kong_2"))))

  and td_1_no_action_1 = lazy (new_state (function
    | No_action _ -> wait_for_draw_in_wall
    | Mahjong _ -> mahjong_declared
    | Pong _ -> player_turn
    | Kong _ -> kong_declared
    | event -> raise (Irrelevent_event (event, "1_no_action_1"))))

  and td_2_pong_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevent_event (event, "2_pong_1"))))

  and td_2_kong_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevent_event (event, "2_kong_1"))))

  and td_1_chow_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | Pong _ -> player_turn
    | Kong _ -> kong_declared
    | event -> raise (Irrelevent_event (event, "1_chow_1"))))

  and td_1_pong_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevent_event (event, "1_pong_1"))))

  and td_1_kong_1 = lazy (new_state (function
    | No_action _ -> kong_declared
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevent_event (event, "1_kong_1"))))

  and mahjong_declared = lazy (new_state (fun _ -> mahjong_declared))

  and kong_declared = lazy (new_state (function
    | Draw _ -> player_turn
    | event -> raise (Irrelevent_event (event, "kong_declared"))))

  and wait_for_kong_robbing = lazy (new_state (fun _ -> wait_for_kong_robbing))
      (*TODO*)

  in
  run action_handler world game_start events
