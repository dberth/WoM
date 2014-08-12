(*Copyright (C) 2014 Denis Berthod*)

open Fsm
open Engine

let rec loop action_handler game state =
  match finished game with
  | Some end_game -> print_endline (string_of_end_game end_game)
  | None ->
    let history = Fsm.history state in
    let events =
      match history with
      | [] -> []
      | Init _ :: tl
      | tl -> Init (known_tiles game) :: tl
    in
    let _, partial_game, _ = build_engine events in
    let possible_actions = accepted_events partial_game state in
    print_endline (string_of_game partial_game);
    let rec read_event_loop () =
      List.iteri
        (fun i event ->
          if i <> 0 && i mod 5 = 0 then print_endline "";
          Printf.printf "%i) %s   " i (string_of_event partial_game event);
        )
        possible_actions;
      print_string "\n>  ";
      flush stdout;
      begin match Scanf.scanf "%i\n" (fun x -> x) with
      | x ->
        if x < 0 || List.length possible_actions <= x then
          read_event_loop ()
        else
          let event = List.nth possible_actions x in
          let event =
            match event with
            | Init [||] -> Init (Array.make 136 None)
            | Wall_breaker_roll _ ->
              let i = Random.int 5 + Random.int 5 + 2 in
              Wall_breaker_roll i
            | Break_wall_roll _ ->
              let i = Random.int 5 + Random.int 5 + 2 in
              Break_wall_roll i
            | _ ->  event
          in
          print_endline (string_of_event partial_game event);
          let game, state = run ~with_history: true action_handler game (lazy state) [event] in
          loop action_handler game state
      | exception Scanf.Scan_failure msg ->
        print_endline ("Scan failure: " ^ msg);
        Scanf.scanf "%s\n" (fun _ -> ());
        read_event_loop ()
      end
      
    in
    read_event_loop ()

let () =
  let action_handler, game, state = build_engine [] in
  loop action_handler game state
    
