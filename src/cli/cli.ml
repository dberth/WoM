(*Copyright (C) 2014 Denis Berthod*)

open Engine

let show_end_game end_game =
  print_endline (string_of_end_game end_game)

let rec make_list x n =
  if n = 0 then
    []
  else
    x :: make_list x (n - 1)

let show_tile tile_descr =
  Printf.sprintf "[%s]" (Tileset.string_of_tile_descr tile_descr)

let show_tileset ?(concealed = false) tileset =
  let tile_descrs = Tileset.tile_descr_of_tileset tileset in
  if concealed && List.length tile_descrs = 4 then
    let tile = Tileset.string_of_tile_descr (List.hd tile_descrs) in
    Printf.sprintf "[XX][%s][%s][XX]" tile tile
  else
    String.concat "" (List.map show_tile tile_descrs)

let show_declared declared =
  let rec aux = function
    | [] -> []
    | (tileset, _, concealed) :: tl ->
      show_tileset ~concealed: true tileset :: aux tl
  in
  print_endline (String.concat " " (aux declared))

let show_discarded_tiles discarded = print_endline (String.concat "" (List.map show_tile discarded))

let show_hand tileset = print_endline (show_tileset tileset)

let show_hand_nb nb =
  let rec aux n =
    if n = 0 then
      []
    else
      let s = string_of_int n in
      let padding = String.make (3 - String.length s) ' ' in
      (" " ^ s ^ padding) :: aux (n - 1)
  in
  print_endline (String.concat "" (List.rev (aux nb)))

let show_discarded_tile = function
  | None -> print_endline ""
  | Some tile_descr ->
      print_endline (show_tile tile_descr)

let show_player viewer player game =
  if player <> 0 then print_endline "--------------------";
  let s1, s2 =
    if player = current_player game then "[","]" else " ", " "
  in
  print_endline (Printf.sprintf "%sPLAYER %i%s:" s1 player s2);
  show_declared (player_declared_sets player game);
  begin
    if player = viewer then begin
      show_hand (player_hand player game);
      show_hand_nb (nb_tiles_in_hand player game)
    end else
      let n = nb_tiles_in_hand player game in
      print_endline (String.concat "" (make_list "[XX]" n));
      print_endline ""
  end;
  show_discarded_tiles (player_discarded_tiles player game)

let show_game viewer game =
  print_endline "====================";
  for i = 0 to 3 do
    show_player viewer i game
  done;
  print_string "DISCARD: ";
  show_discarded_tile (discarded_tile game);
  print_endline ""

let read_event events = List.hd events

let human_player_event possible_actions game state =
  match possible_actions with
  | [] -> assert false
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

let rec loop human_players action_handler game state =
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
    show_game 0 game;
    let possible_actions = Fsm.accepted_events game state in
    let event =
      match possible_actions with
      | [] -> assert false
      | [Init [||]] -> Init random_game
      | [Wall_breaker_roll _] ->
        let i = Random.int 5 + Random.int 5 + 2 in
        print_endline (Printf.sprintf "Roll a %i." i);
        Wall_breaker_roll i
      | [Break_wall_roll _] ->
        let i = Random.int 5 + Random.int 5 + 2 in
        print_endline (Printf.sprintf "Roll a %i." i);
        Break_wall_roll i
      | _ ->
        let current_player = current_player game in
        if List.mem current_player human_players then
          human_player_event possible_actions game state
        else
          Mahjong_ai.mc_ai_with_bias ~evaluate_game ~nb_trajectory events 0.8
    in
    let game, state = Fsm.run ~with_history: true action_handler game (lazy state) [event] in
    loop human_players action_handler game state

let () =
  Random.self_init ();
  let action_handler, game, state = build_engine [] in
  loop [] action_handler game state
