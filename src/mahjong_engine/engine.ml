(*Copyright (C) 2014 Denis Berthod*)

open Tileset
open Fsm

type player = int

type tile_pos = int (*A position in the initial array*)

type event =
  | Init of tile array option
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

let build_engine

    ?on_wait_for_wall_breaker_roll_entry
    ?on_wait_for_break_roll_entry
    ?on_wait_for_deal_entry
    ?on_wait_for_draw_in_wall_entry
    ?on_player_turn_entry
    ?on_tile_discarded_entry
    ?on_td_1_no_action_2_entry
    ?on_td_1_chow_2_entry
    ?on_td_1_pong_2_entry
    ?on_td_1_kong_2_entry
    ?on_td_1_no_action_1_entry
    ?on_td_2_pong_1_entry
    ?on_td_2_kong_1_entry
    ?on_td_1_chow_1_entry
    ?on_td_1_pong_1_entry
    ?on_td_1_kong_1_entry
    ?on_mahjong_declared_entry
    ?on_kong_declared_entry
    ?on_wait_for_kong_robbing_entry
    ?on_kr_2_entry
    ?on_kr_1_entry

    ?on_game_start_exit
    ?on_wait_for_wall_breaker_roll_exit
    ?on_wait_for_break_roll_exit
    ?on_wait_for_deal_exit
    ?on_wait_for_draw_in_wall_exit
    ?on_player_turn_exit
    ?on_tile_discarded_exit
    ?on_td_1_no_action_2_exit
    ?on_td_1_chow_2_exit
    ?on_td_1_pong_2_exit
    ?on_td_1_kong_2_exit
    ?on_td_1_no_action_1_exit
    ?on_td_2_pong_1_exit
    ?on_td_2_kong_1_exit
    ?on_td_1_chow_1_exit
    ?on_td_1_pong_1_exit
    ?on_td_1_kong_1_exit
    ?on_mahjong_declared_exit
    ?on_kong_declared_exit
    ?on_wait_for_kong_robbing_exit
    ?on_kr_2_exit
    ?on_kr_1_exit



    () = 
  let rec game_start = lazy (new_state (function
    | Init _ -> wait_for_wall_breaker_roll
    | event -> raise (Irrelevant_event (event, "game_start"))))

  and wait_for_wall_breaker_roll = lazy (new_state (function
    | Wall_breaker_roll x when (1 < x && x <= 12) -> wait_for_break_roll
    | event -> raise (Irrelevant_event (event, "wait_for_wall_break_roll"))))

  and wait_for_break_roll = lazy (new_state (function
    | Break_wall_roll x when (1 < x && x <= 12) -> wait_for_deal
    | event -> raise (Irrelevant_event (event, "wait_for_break_roll"))))

  and wait_for_deal = lazy (new_state (function
    | Deal -> wait_for_draw_in_wall
    | event -> raise (Irrelevant_event (event, "wait_for_deal"))))

  and wait_for_draw_in_wall = lazy (new_state (function
    | Draw _ -> player_turn
    | event -> raise (Irrelevant_event (event, "wait_for_draw_in_wall"))))

  and player_turn = lazy (new_state (function
    | Discard _ -> tile_discarded
    | Mahjong _ -> mahjong_declared
    | Concealed_kong _ -> kong_declared
    | Small_kong _ -> wait_for_kong_robbing
    | event -> raise (Irrelevant_event (event, "player_turn"))))

  and tile_discarded = lazy (new_state (function
    | No_action _ -> td_1_no_action_2
    | Mahjong _ -> mahjong_declared
    | Chow _ -> td_1_chow_2
    | Pong _ -> td_1_pong_2
    | Kong _ -> td_1_kong_2
    | event -> raise (Irrelevant_event (event, "tile_discarded"))))

  and td_1_no_action_2 = lazy (new_state (function
    | No_action _ -> td_1_no_action_1
    | Mahjong _ -> mahjong_declared
    | Pong _ -> td_2_pong_1
    | Kong _ -> td_2_kong_1
    | event -> raise (Irrelevant_event (event, "1_no_action_2"))))

  and td_1_chow_2 = lazy (new_state (function
    | No_action _ -> td_1_chow_1
    | Mahjong _ -> mahjong_declared
    | Pong _ -> td_2_pong_1
    | Kong _ -> td_2_kong_1
    | event -> raise (Irrelevant_event (event, "1_chow_2"))))

  and td_1_pong_2 = lazy (new_state (function
    | No_action _ -> td_1_pong_1
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevant_event (event, "1_pong_2"))))

  and td_1_kong_2 = lazy (new_state (function
    | No_action _ -> td_1_kong_1
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevant_event (event, "1_kong_2"))))

  and td_1_no_action_1 = lazy (new_state (function
    | No_action _ -> wait_for_draw_in_wall
    | Mahjong _ -> mahjong_declared
    | Pong _ -> player_turn
    | Kong _ -> kong_declared
    | event -> raise (Irrelevant_event (event, "1_no_action_1"))))

  and td_2_pong_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevant_event (event, "2_pong_1"))))

  and td_2_kong_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevant_event (event, "2_kong_1"))))

  and td_1_chow_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | Pong _ -> player_turn
    | Kong _ -> kong_declared
    | event -> raise (Irrelevant_event (event, "1_chow_1"))))

  and td_1_pong_1 = lazy (new_state (function
    | No_action _ -> player_turn
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevant_event (event, "1_pong_1"))))

  and td_1_kong_1 = lazy (new_state (function
    | No_action _ -> kong_declared
    | Mahjong _ -> mahjong_declared
    | event -> raise (Irrelevant_event (event, "1_kong_1"))))

  and mahjong_declared = lazy (new_state (fun _ -> mahjong_declared))

  and kong_declared = lazy (new_state (function
    | Draw _ -> player_turn
    | event -> raise (Irrelevant_event (event, "kong_declared"))))

  and wait_for_kong_robbing = lazy (new_state (function
    | Mahjong _ -> mahjong_declared
    | No_action _ -> kr_2
    | event -> raise (Irrelevant_event (event, "wait_for_kong_robbing"))))

  and kr_2 = lazy (new_state (function
    | Mahjong _ -> mahjong_declared
    | No_action _ -> kr_1
    | event -> raise (Irrelevant_event (event, "kr_2"))))

  and kr_1 = lazy (new_state (function
    | Mahjong _ -> mahjong_declared
    | No_action _ -> kong_declared
    | event -> raise (Irrelevant_event (event, "kr_1"))))

  in
  let opt f state action_opt action_handler =
    match action_opt with
    | None -> action_handler
    | Some action -> f state action action_handler
  in
  let on_entry = opt on_entry in
  let on_exit = opt on_exit in
  let action_handler =
    empty_action_handler |>
      on_exit game_start on_game_start_exit |>
      on_entry wait_for_wall_breaker_roll on_wait_for_wall_breaker_roll_entry |>
      on_entry wait_for_break_roll on_wait_for_break_roll_entry |>
      on_entry wait_for_deal on_wait_for_deal_entry |>
      on_entry wait_for_draw_in_wall on_wait_for_draw_in_wall_entry |>
      on_entry player_turn on_player_turn_entry |>
      on_entry tile_discarded on_tile_discarded_entry |>
      on_entry td_1_no_action_2 on_td_1_no_action_2_entry |>
      on_entry td_1_chow_2 on_td_1_chow_2_entry |>
      on_entry td_1_pong_2 on_td_1_pong_2_entry |>
      on_entry td_1_kong_2 on_td_1_kong_2_entry |>
      on_entry td_1_no_action_1 on_td_1_no_action_1_entry |>
      on_entry td_2_pong_1 on_td_2_pong_1_entry |>
      on_entry td_2_kong_1 on_td_2_kong_1_entry |>
      on_entry td_1_chow_1 on_td_1_chow_1_entry |>
      on_entry td_1_pong_1 on_td_1_pong_1_entry |>
      on_entry td_1_kong_1 on_td_1_kong_1_entry |>
      on_entry mahjong_declared on_mahjong_declared_entry |>
      on_entry kong_declared on_kong_declared_entry |>
      on_entry wait_for_kong_robbing on_wait_for_kong_robbing_entry |>
      on_entry kr_2 on_kr_2_entry |>
      on_entry kr_1 on_kr_1_entry |>

      on_exit wait_for_wall_breaker_roll on_wait_for_wall_breaker_roll_exit |>
      on_exit wait_for_break_roll on_wait_for_break_roll_exit |>
      on_exit wait_for_deal on_wait_for_deal_exit |>
      on_exit wait_for_draw_in_wall on_wait_for_draw_in_wall_exit |>
      on_exit player_turn on_player_turn_exit |>
      on_exit tile_discarded on_tile_discarded_exit |>
      on_exit td_1_no_action_2 on_td_1_no_action_2_exit |>
      on_exit td_1_chow_2 on_td_1_chow_2_exit |>
      on_exit td_1_pong_2 on_td_1_pong_2_exit |>
      on_exit td_1_kong_2 on_td_1_kong_2_exit |>
      on_exit td_1_no_action_1 on_td_1_no_action_1_exit |>
      on_exit td_2_pong_1 on_td_2_pong_1_exit |>
      on_exit td_2_kong_1 on_td_2_kong_1_exit |>
      on_exit td_1_chow_1 on_td_1_chow_1_exit |>
      on_exit td_1_pong_1 on_td_1_pong_1_exit |>
      on_exit td_1_kong_1 on_td_1_kong_1_exit |>
      on_exit mahjong_declared on_mahjong_declared_exit |>
      on_exit kong_declared on_kong_declared_exit |>
      on_exit wait_for_kong_robbing on_wait_for_kong_robbing_exit |>
      on_exit kr_2 on_kr_2_exit |>
      on_exit kr_1 on_kr_1_exit

  in
  fun world events ->
    run action_handler world game_start events
