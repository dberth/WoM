(*Copyright (C) 2014 Denis Berthod*)

open Fsm
open Engine

let rec loop action_handler game state =
  match finished game with
  | Some end_game -> print_endline (string_of_end_game end_game)
  | None ->
    let possible_actions = accepted_events game state in
    print_endline (string_of_game game);
    let rec read_event_loop () =
      List.iteri
        (fun i event ->
          print_endline ((Printf.sprintf "%i) %s" i (string_of_event event)))
        )
        possible_actions;
      begin match Scanf.scanf "%i" (fun x -> x) with
      | x ->
         if x < 0 || List.length possible_actions <= x then
          read_event_loop ()
        else
          let event = List.nth possible_actions x in
          let game, state = run action_handler game (lazy state) [event] in
          loop action_handler game state
      | exception _ -> read_event_loop ()
      end
      
    in
    read_event_loop ()

let () =
  let action_handler, game, state = build_engine [] in
  loop action_handler game state
    
