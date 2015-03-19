(*Copyright (C) 2014 Denis Berthod*)

open Engine
open Game_descr

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

let show_player visibles player round =
  if player <> 0 then print_endline "--------------------";
  let s1, s2 =
    if player = current_player round then
      "[","]"
    else if Some player = discard_player round then
      "<", ">"
    else
      " ", " "
  in
  print_endline (Printf.sprintf "%sPLAYER %i%s:" s1 player s2);
  show_declared (player_declared_sets player round);
  begin
    if List.mem player visibles then begin
      let current = last_drawn_tile player round in
      show_hand ?current (player_hand player round);
      show_hand_nb (nb_tiles_in_hand player round)
    end else
      let n = nb_tiles_in_hand player round in
      print_endline (String.concat "" (make_list "[XX]" n));
  end;
  print_string "DISCARD: ";
  show_discarded_tile player (discard_player round) (discarded_tile round);
  show_discarded_tiles (player_discarded_tiles player round)

let show_round visibles round =
  print_endline "====================";
  for i = 0 to 3 do
    show_player visibles i round
  done

let show_end_round round =
  show_round [0; 1; 2; 3] round;
  match finished round with
  | None -> assert false
  | Some No_winner -> print_endline "==== DRAW GAME ==="
  | _ ->
    let hand_explanation, score = Rule_manager.explain_hand_score round in
    let hand_explanation = List.sort (fun (_, x) (_, y) -> compare x y) hand_explanation in
    let hand_explanations =
      String.concat "\n"
        (List.map
           (fun (s, v) ->
              Printf.sprintf "%s: %.0f" s v
           )
           hand_explanation
        )
    in
    let player_explanations =
      String.concat "\n"
        (List.map
           (fun player ->
              let s, v = Rule_manager.explain_player_score player round ~hand_score: score in
              Printf.sprintf "%s: %.0f" s v
           )
           [0; 1; 2; 3]
        )
    in
    print_endline
      (Printf.sprintf "==== PLAYER %i WINS WITH %.0f PTS===\n%s\n====\n%s\n"
         (current_player round)
         score
         hand_explanations
         player_explanations
      )
         

let string_of_tile_pos round pos =
  Tileset.string_of_tile_descr (descr_of_tile_pos round pos)

let compare_discard_events round e1 e2 =
  match e1, e2 with
  | Discard(_, pos1), Discard(_, pos2) ->
    compare (tile_of_tile_pos round pos1) (tile_of_tile_pos round pos2)
  | _ -> assert false

let duplicate_events round events tiles =
  let table = Hashtbl.create 4 in
  List.iter
    (fun event ->
       match event with
       | Discard (_, pos) ->
         Hashtbl.add table (tile_of_tile_pos round pos) event
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

let discard_events round events hand =
  let events =
    List.filter (function Discard _ -> true | _ -> false) events
  in
  match events with
  | [] -> []
  | _ ->
    let tiles = List.sort Tileset.compare_tiles (Tileset.tiles_of_tileset hand) in
    duplicate_events round events tiles

let make_round_descr round state =
  let events = Fsm.history state in
  {current_round = Engine.set_real_init_tiles events round; game_events = []}

let read_event round events round_descr =
  let hand = current_player_hand round in
  let rec loop () =
    print_string "> "; flush stdout;
    begin match Scanf.scanf "%i\n" (fun x -> x) with
    | i ->
      let discard_events = discard_events round events hand in
      if 0 < i && i <= List.length discard_events then
        List.nth discard_events (i - 1)
      else
        bad_move ()
    | exception Scanf.Scan_failure _ ->
      match Scanf.scanf "%s\n" (fun x -> x) with
      | "" -> send_event (No_action (current_player round))
      | "m" | "M" -> send_event (Mahjong (current_player round))
      | "k" | "K" -> kong_loop ()
      | "p" | "P" -> pong_loop ()
      | "c" | "C" -> chow_loop ()
      | "dump" ->
        Game_descr.dump round_descr "wom_dump.bak";
        loop ()
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
          print_endline (Printf.sprintf "%i) %s" (i + 1) (String.concat "" (List.map  (fun pos -> Printf.sprintf "[%s]" (string_of_tile_pos round pos)) (get_set event))))
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
      if 0 < i && i <= List.length events then
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

let human_player_event possible_actions round state =
  match possible_actions with
  | [] -> assert false
  | [Init _ as x] | [Break_wall_roll _ as x] | [Wall_breaker_roll _ as x] | [Deal as x] | [Draw _ as x]-> x 
  | _ ->
    let round_descr = make_round_descr round state in
    read_event round possible_actions round_descr

let rec loop human_players action_handler round state =
  let nb_trajectory = 1_000 in
  match finished round with
  | Some _ -> show_end_round round
  | None ->
    let history = Fsm.history state in
    let events =
      match history with
      | [] -> []
      | Init _ :: tl
      | tl -> Init (known_tiles round) :: tl
    in
    
    show_round human_players round;
    let possible_actions = Fsm.accepted_events round state in
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
        let current_player = current_player round in
        if List.mem current_player human_players then
          human_player_event possible_actions round state
        else
          let evaluate_round player round = Rule_manager.evaluate_round player round in
          let irregular_hands = Rule_manager.irregular_hands () in
          let seven_pairs = Rule_manager.seven_pairs () in
          Mahjong_ai.mc_ai_with_bias ~irregular_hands ~seven_pairs ~evaluate_round ~nb_trajectory events 0.8
    in
    let round, state = Fsm.run ~with_history: true action_handler round (lazy state) [event] in
    let round_descr = make_round_descr round state in
    Game_descr.dump round_descr "round_dump.bak";
    loop human_players action_handler round state

let () =
  Rules.Loader.load_rules ();
  Rule_manager.set_default_rule ();
  Random.self_init ();
  let irregular_hands = Rule_manager.irregular_hands () in
  let seven_pairs = Rule_manager.seven_pairs () in
  let initial_events =
    if Array.length Sys.argv <= 1 then
      []
    else
      let dump_file = Sys.argv.(1) in
      let {Game_descr.current_round; _} = Game_descr.restore dump_file in
      current_round
  in
  let action_handler, round, state = build_engine ~irregular_hands ~seven_pairs initial_events in
  loop [0] action_handler round state
