(*Copyright (C) 2014 Denis Berthod*)

open Engine

let show_end_game end_game =
  print_endline (string_of_end_game end_game)

let show_game game =
  print_endline "====================";
  print_endline (string_of_game game)

let read_event events = List.hd events

let player_event possible_actions game state =
  match possible_actions with
  | [] -> assert false
  | [Init [||]] -> Init (Array.make 136 None)
  | [Wall_breaker_roll _] ->
    let i = Random.int 5 + Random.int 5 + 2 in
    print_endline (Printf.sprintf "Roll a %i." i);
    Wall_breaker_roll i
  | [Break_wall_roll _] ->
    let i = Random.int 5 + Random.int 5 + 2 in
    print_endline (Printf.sprintf "Roll a %i." i);
    Break_wall_roll i
  | [x] -> x
  | _ -> read_event possible_actions


let evaluate_game player game =
  match finished game with
  | None -> assert false
  | Some No_winner -> 0.
  | Some (Mahjong _) ->
    if current_player game = player then
      1.
    else
      -1.

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
    show_game game;
    begin
      let known_tiles = known_tiles game in
      let i = ref (-1) in
      let known_tiles =
        Array.fold_left
          (fun acc tile ->
            incr i;
            match tile with
            | None ->  acc
            | Some tile -> Printf.sprintf "(%i, %s)" !i (Tileset.string_of_tile_descr tile) :: acc
          )
          []
          known_tiles
      in
      print_endline (String.concat "; " known_tiles)
    end;
    let possible_actions = Fsm.accepted_events game state in
    let event =
      let current_player = current_player game in
      if current_player = 0 then
        player_event possible_actions game state
      else
        Mahjong_ai.mc_ai_with_bias ~evaluate_game ~nb_trajectory events 0.8
    in
    begin match event with
    | Init _ | Break_wall_roll _ | Wall_breaker_roll _ -> ()
    | _ ->
      if not (List.mem event possible_actions) then begin
        print_endline (string_of_event game event);
        print_endline (String.concat "; " (List.map (string_of_event game) possible_actions));
        assert false
      end
    end;
    let game, state = Fsm.run ~with_history: true action_handler game (lazy state) [event] in
    loop action_handler game state

let () =
  Random.self_init ();
  let action_handler, game, state = build_engine [] in
  loop action_handler game state
