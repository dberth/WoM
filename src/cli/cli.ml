(*Copyright (C) 2014 Denis Berthod*)

open Engine

let show_end_game end_game =
  print_endline (string_of_end_game end_game)

let show_game game =
  print_endline (string_of_game game)

let player_event game = assert false (*TODO*)

let evaluate_game player game =
  match finished game with
  | None -> assert false
  | Some No_winner -> 0.5
  | Some (Mahjong _) ->
    if current_player game = player then
      1.
    else
      0.

let rec loop action_handler game state =
  let nb_trajectory = 1_000 in
  match finished game with
  | Some end_game -> show_end_game end_game
  | None ->
    let history = Fsm.history state in
    let events =
      match history with
      | [] -> []
      | Init _ :: tl
      | tl -> Init (known_tiles game) :: tl
    in
    let _, partial_game, _ = build_engine events in
    show_game partial_game;
    let event =
      let current_player = current_player partial_game in
      if  current_player = 0 then
        player_event game
      else
        Mahjong_ai.mc_ai_with_bias ~evaluate_game ~nb_trajectory events 0.8
    in
    let game, state = Fsm.run ~with_history: true action_handler game (lazy state) [event] in
    loop action_handler game state
