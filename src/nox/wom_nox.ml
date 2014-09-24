(*Copyright (C) 2014 Denis Berthod*)

open Engine

let rec make_list x n =
  if n = 0 then
    []
  else
    x :: make_list x (n - 1)

let show_tile ?current tile =
  let sep1, sep2 =
    match current with
    | Some t when t = tile -> "<", ">"
    | _ -> "[", "]"
  in
  Printf.sprintf "%s%s%s" sep1 (Tileset.string_of_tile tile) sep2

let show_tileset ?current ?(sorted = false) ?(concealed = false) tileset =
  let tiles = Tileset.tiles_of_tileset tileset in
  if concealed && List.length tiles = 4 then
    let tile = Tileset.string_of_tile (List.hd tiles) in
    Printf.sprintf "[XX][%s][%s][XX]" tile tile
  else
    let tiles =
      if sorted then List.sort Tileset.compare_tiles tiles else tiles
    in
    let tiles = List.map (show_tile ?current) tiles in
    String.concat "" tiles

let show_declared declared =
  let rec aux = function
    | [] -> []
    | (tileset, _, concealed) :: tl ->
      show_tileset ~concealed tileset :: aux tl
  in
  print_endline (String.concat " " (aux declared))

let show_discarded_tiles discarded = print_endline (String.concat "" (List.map show_tile discarded))

let show_hand ?current tileset = print_endline (show_tileset ?current ~sorted: true tileset)

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

let show_discarded_tile player discard_player = function
  | None -> print_endline ""
  | Some tile ->
    if Some player = discard_player then
      print_endline (show_tile tile)
    else
      print_endline ""

let show_player visibles player game =
  if player <> 0 then print_endline "--------------------";
  let s1, s2 =
    if player = current_player game then
      "[","]"
    else if Some player = discard_player game then
      "<", ">"
    else
      " ", " "
  in
  print_endline (Printf.sprintf "%sPLAYER %i%s:" s1 player s2);
  show_declared (player_declared_sets player game);
  begin
    if List.mem player visibles then begin
      let current = last_drawn_tile player game in
      show_hand ?current (player_hand player game);
      show_hand_nb (nb_tiles_in_hand player game)
    end else
      let n = nb_tiles_in_hand player game in
      print_endline (String.concat "" (make_list "[XX]" n));
  end;
  print_string "DISCARD: ";
  show_discarded_tile player (discard_player game) (discarded_tile game);
  show_discarded_tiles (player_discarded_tiles player game)

let show_game visibles game =
  print_endline "====================";
  for i = 0 to 3 do
    show_player visibles i game
  done

let show_end_game game =
  show_game [0; 1; 2; 3] game;
  match finished game with
  | None -> assert false
  | Some No_winner -> print_endline "==== DRAW GAME ==="
  | _ ->
    let player = current_player game in
    print_endline (Printf.sprintf "==== PLAYER %i WINS %.0f PTS===" player (Rule_manager.evaluate_game player game))

let string_of_tile_pos game pos =
  Tileset.string_of_tile_descr (descr_of_tile_pos game pos)

let compare_discard_events game e1 e2 =
  match e1, e2 with
  | Discard(_, pos1), Discard(_, pos2) ->
    compare (tile_of_tile_pos game pos1) (tile_of_tile_pos game pos2)
  | _ -> assert false

let duplicate_events game events tiles =
  let table = Hashtbl.create 4 in
  List.iter
    (fun event ->
       match event with
       | Discard (_, pos) ->
         Hashtbl.add table (tile_of_tile_pos game pos) event
       | _ -> assert false
    )
    events;
  List.map
    (fun tile ->
       match Hashtbl.find table tile with
       | event -> event
       | exception Not_found -> assert false
    )
    tiles

let discard_events game events hand =
  let events =
    List.filter (function Discard _ -> true | _ -> false) events
  in
  match events with
  | [] -> []
  | _ ->
    let tiles = List.sort Tileset.compare_tiles (Tileset.tiles_of_tileset hand) in
    duplicate_events game events tiles

let read_event game events =
  let hand = current_player_hand game in
  let rec loop () =
    print_string "> "; flush stdout;
    begin match Scanf.scanf "%i\n" (fun x -> x) with
    | i ->
      let discard_events = discard_events game events hand in
      if 0 < i && i <= List.length discard_events then
        List.nth discard_events (i - 1)
      else
        bad_move ()
    | exception Scanf.Scan_failure _ ->
      match Scanf.scanf "%s\n" (fun x -> x) with
      | "" -> send_event (No_action (current_player game))
      | "m" | "M" -> send_event (Mahjong (current_player game))
      | "k" | "K" -> kong_loop ()
      | "p" | "P" -> pong_loop ()
      | "c" | "C" -> chow_loop ()
      | _ -> bad_move ()
    end
  and bad_move () =
    print_endline "Bad move. Retry.";
    loop ()
  and send_event event =
    if List.mem event events then
      event
    else
      bad_move ()
  and declare_event filter get_set =
    let events = List.filter filter events in
    match events with
    | [] -> bad_move ()
    | [x] -> x
    | _ ->
      List.iteri
        (fun i event ->
          print_endline (Printf.sprintf "%i) %s" (i + 1) (String.concat "" (List.map  (fun pos -> Printf.sprintf "[%s]" (string_of_tile_pos game pos)) (get_set event))))
        )
        events;
      declare_loop events
  and bad_declare_move events  =
    print_endline "Bad move.Retry.";
    declare_loop events

  and declare_loop events =
    print_string "> "; flush stdout;
    begin match Scanf.scanf "%i\n" (fun x -> x) with
    | i ->
      if 0 < i && i < List.length events then
        List.nth events (i - 1)
      else
        bad_declare_move events
    | exception Scanf.Scan_failure _ ->
      ignore (Scanf.scanf "%s\n" (fun x -> x));
      bad_declare_move events
    end
      
  and kong_loop () =
    declare_event
      (function
        | Concealed_kong _ | Small_kong _ | Kong _ -> true
        | _ -> false
      )
      (function
        | Concealed_kong (_, positions)
        | Kong (_, positions) -> positions
        | Small_kong (_, pos) -> [pos; pos; pos; pos]
        | _ -> assert false
      )
  and pong_loop () =
    declare_event
      (function Pong _ -> true | _ -> false)
      (function Pong (_, positions) -> positions | _ -> assert false)
  and chow_loop () =
    declare_event
      (function Chow _ -> true | _ -> false)
      (function Chow (_, positions) -> positions | _ -> assert false)
  in
  loop ()

let human_player_event possible_actions game state =
  match possible_actions with
  | [] -> assert false
  | [Init _ as x] | [Break_wall_roll _ as x] | [Wall_breaker_roll _ as x] | [Deal as x] | [Draw _ as x]-> x 
  | _ -> read_event game possible_actions


let rec loop human_players action_handler game state =
  let nb_trajectory = 1_000 in
  match finished game with
  | Some _ -> show_end_game game
  | None ->
    let history = Fsm.history state in
    let events =
      match history with
      | [] -> []
      | Init _ :: tl
      | tl -> Init (known_tiles game) :: tl
    in
    show_game human_players game;
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
          let evaluate_game player game = Rule_manager.evaluate_game player game in
          let irregular_hands = Rule_manager.irregular_hands () in
          let seven_pairs = Rule_manager.seven_pairs () in
          Mahjong_ai.mc_ai_with_bias ~irregular_hands ~seven_pairs ~evaluate_game ~nb_trajectory events 0.8
    in
    let game, state = Fsm.run ~with_history: true action_handler game (lazy state) [event] in
    loop human_players action_handler game state

let () =
  Simple.register ();
  Zung_jung.register ();
  Rule_manager.set_default_rule ();
  Random.self_init ();
  let irregular_hands = Rule_manager.irregular_hands () in
  let seven_pairs = Rule_manager.seven_pairs () in
  let action_handler, game, state = build_engine ~irregular_hands ~seven_pairs [] in
  loop [0] action_handler game state
