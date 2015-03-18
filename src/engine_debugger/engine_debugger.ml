(*Copyright (C) 2014 Denis Berthod*)

open Fsm
open Engine
open Game_descr

let rec loop action_handler round state =
  match finished round with
  | Some end_round -> print_endline (string_of_end_round end_round)
  | None ->
    (* let history = Fsm.history state in *)
    (* let events = *)
    (*   match history with *)
    (*   | [] -> [] *)
    (*   | Init _ :: tl *)
    (*   | tl -> Init (known_tiles round) :: tl *)
    (* in *)
    (* let _, partial_round, _ = build_engine events in *)
    let partial_round = round in
    let possible_actions = accepted_events partial_round state in
    print_endline (string_of_round partial_round);
    let rec read_event_loop () =
      List.iteri
        (fun i event ->
          if i <> 0 && i mod 5 = 0 then print_endline "";
          Printf.printf "%i) %s   " i (string_of_event partial_round event);
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
          print_endline (string_of_event partial_round event);
          let round, state = run ~with_history: true action_handler round (lazy state) [event] in
          loop action_handler round state
      | exception Scanf.Scan_failure msg ->
        print_endline ("Scan failure: " ^ msg);
        Scanf.scanf "%s\n" (fun _ -> ());
        read_event_loop ()
      end
      
    in
    read_event_loop ()

let () =
  let action_handler, round, state = build_engine ~seven_pairs: false [] in
  loop action_handler round state
    
