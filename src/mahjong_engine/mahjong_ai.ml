(*Copyright (C) 2014 Denis Berthod*)

open Engine
open Game_descr

let rec remove_empty = function
  | [] -> []
  | [] :: tl -> remove_empty tl
  | hd :: tl -> hd :: remove_empty tl 

let apply_bias bias buckets =
  let rec aux = function
    | [] -> failwith "No buckets"
    | [[x]] -> x
    | [set] -> List.nth set (Random.int (List.length set))
    | hd :: tl ->
      if Random.float 1. < bias then begin
        List.nth hd (Random.int (List.length hd))
      end else
        aux tl
  in
  aux (remove_empty buckets)


let mc_next_event_with_bias round state bias =
  let possible_actions = Fsm.accepted_events round state in
  match possible_actions with
  | [] ->
    List.iter
      (fun event ->
        print_endline (string_of_event round event)
      )
      (Fsm.history state);
    assert false
  | [unique_action] -> unique_action
  | _ ->
    let high_priority_actions =
      List.filter
        (fun event ->
          match (event: Game_descr.round_event) with
          | Mahjong _ | Concealed_kong _ | Small_kong _ -> true
          | _ -> false
        )
        possible_actions
    in
    match high_priority_actions with
    | hd :: _ -> hd
    | [] ->
      let discard_actions =
        List.filter
          (function
          | Chow _ | Pong _ | Kong _ -> true
          | _ -> false
          )
          possible_actions
      in
      match discard_actions with
      | _ :: _ ->
        begin try
          apply_bias bias [discard_actions; [No_action (current_player round)]]
        with
        | Failure _ -> print_endline (String.concat "; " (List.map (string_of_event round) discard_actions)); assert false
        end
      | _ ->
        let {Tileset.alone; in_sub_chow; in_pair; in_3set} = Tileset.status_of_tileset (current_player_hand round) in
        let tile_descr = apply_bias bias [alone; in_sub_chow; in_pair; in_3set] in
        try
          List.find
            (function
              | Discard(_, tile_pos) ->
                descr_of_tile_pos round tile_pos = tile_descr
              | _ -> false
            )
            possible_actions
        with
        | Not_found ->
          print_endline "===";
          print_endline (Tileset.string_of_tile_descr tile_descr);
          List.iter (fun event -> print_endline (string_of_round_event event)) possible_actions;
          print_endline "=====";
          assert false

let mc_trajectory_with_bias ?irregular_hands ~seven_pairs ~event_history ~possible_actions bias =
  let action_handler, round, state = build_engine ?irregular_hands ~seven_pairs event_history in
  let chosen_event = List.nth possible_actions (Random.int (List.length possible_actions)) in
  let round, state = Fsm.run action_handler round (lazy state) [chosen_event] in
  let rec loop round state =
    match finished round with
    | Some _ -> round, chosen_event
    | None ->
      let event = mc_next_event_with_bias round state bias in
      let round, state = Fsm.run (*~with_history: true*) action_handler round (lazy state) [event] in
      loop round state
  in
  loop round state

let apply_score possible_actions_tab action score =
  let (nb, sum) = try Hashtbl.find possible_actions_tab action with Not_found -> assert false in
  Hashtbl.replace possible_actions_tab action (nb +. 1., sum +. score)

let apply_amaf possible_actions_tab possible_actions chosen_action round score =
  List.iter
    (function
      | Discard (_, tile_pos) as action ->
        if not (is_in_current_player_hand round tile_pos) then
          apply_score possible_actions_tab action score
      | action when action = chosen_action ->
        apply_score possible_actions_tab action score
      | _ -> ()
    )
    possible_actions

let mc_ai_with_bias ?(debug = false) ?irregular_hands ~seven_pairs ~evaluate_round ~nb_trajectory event_history bias =
  let _, round, state = build_engine ?irregular_hands ~seven_pairs event_history in
  let possible_actions = Fsm.accepted_events round state in
  let possible_actions_tab = Hashtbl.create 14 in
  List.iter (fun x -> Hashtbl.add possible_actions_tab x (0., 0.)) possible_actions;
  match possible_actions with
  | [] -> assert false
  | [unique_action] -> unique_action
  | _ ->
    let player = current_player round in
    for _ = 1 to nb_trajectory do
      let round, chosen_action = mc_trajectory_with_bias ?irregular_hands ~seven_pairs ~event_history ~possible_actions bias in
      let score = evaluate_round player round in
      apply_amaf possible_actions_tab possible_actions chosen_action round score;
    done;
    let result =
      Hashtbl.fold
        (fun event (nb, sum) result ->
          match result with
          | None ->
            let average = sum /. nb in
            if debug then print_endline (Printf.sprintf "%.2f, %s" average (string_of_event round event));
            Some (average, event)
          | Some (old_average, old_event) ->
            let average = sum /. nb in
            if debug then print_endline (Printf.sprintf "%.2f, %s" average (string_of_event round event));
            if old_average <  average then
              Some (average, event)
            else
              Some (old_average, old_event)
        )
        possible_actions_tab
        None
    in
    if debug then Scanf.scanf "%s\n" (fun _ -> ());
    match result with
    | None -> assert false
    | Some (_, event) -> event
  
