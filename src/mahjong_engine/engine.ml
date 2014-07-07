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

  and tile_discarded = lazy (new_state (fun _ -> tile_discarded))
(*TODO*)

  and mahjong_declared = lazy (new_state (fun _ -> mahjong_declared))

  and kong_declared = lazy (new_state (fun _ -> kong_declared))
  (*TODO*)

  and wait_for_kong_robbing = lazy (new_state (fun _ -> wait_for_kong_robbing))

  in
  run action_handler world game_start events
