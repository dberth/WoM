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

  and wait_for_deal = lazy (new_state (function _ -> wait_for_deal))
(*TODO*)

  in
  run action_handler world game_start events
