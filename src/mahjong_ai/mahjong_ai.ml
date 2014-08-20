(*Copyright (C) 2014 Denis Berthod*)

open Engine

let rec apply_bias bias = function
  | [] -> assert false
  | [[x]] -> x
  | [set] -> List.nth set (Random.int (List.length set))
  | hd :: tl ->
    if Random.float 1. < bias then
      List.nth hd (Random.int (List.length hd))
    else
      apply_bias bias tl

let mc_next_event_with_bias game state bias =
  let possible_actions = Fsm.accepted_events game state in
  match possible_actions with
  | [unique_action] -> unique_action
  | _ ->
    let high_priority_actions =
      List.filter
        (function
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
        apply_bias bias [discard_actions; [No_action (current_player game)]]
      | _ ->
        let {Tileset.alone; in_sub_chow; in_pair; in_3set} = Tileset.status_of_tileset (current_player_hand game) in
        let tile_descr = apply_bias bias [alone; in_sub_chow; in_pair; in_3set] in
        try
          List.find
            (function
              | Discard(_, tile_pos) ->
                descr_of_tile_pos game tile_pos = tile_descr
              | _ -> false
            )
            possible_actions
        with
        | Not_found -> assert false

let mc_trajectory_with_bias event_history bias =
  let action_handler, game, state = build_engine event_history in
  let rec loop game state =
    match finished game with
    | Some _ -> game, state
    | None ->
      let event = mc_next_event_with_bias game state bias in
      let game, state = Fsm.run action_handler game (lazy state) [event] in
      loop game state
  in
  loop game state

let mc_ai_with_bias ~evaluate_game ~nb_trajectory event_history bias =
  let _, game, state = build_engine event_history in
  let possible_actions_list = Fsm.accepted_events game state in
  match possible_actions_list with
  | [] -> assert false
  | [unique_action] -> unique_action
  | _ ->
    let possible_actions = Hashtbl.create 14 in
    List.iter (fun x -> Hashtbl.add possible_actions x (0., 0.)) possible_actions_list;
    let player = current_player game in
    for i = 1 to nb_trajectory do
      let game, state = mc_trajectory_with_bias event_history bias in
      let score = evaluate_game player game in
      let event_history = Fsm.history state in
      List.iter
        (fun event ->
          match Hashtbl.find possible_actions event with
          | (nb, sum) -> Hashtbl.replace possible_actions event (nb +. 1., sum +. score)
          | exception Not_found -> ()
        )
        event_history
    done;
    let result =
      Hashtbl.fold
        (fun event (nb, sum) result ->
          match result with
          | None -> Some (sum /. nb, event)
          | Some (old_average, old_event) ->
            let average = sum /. nb in
            if old_average <  average then
              Some (average, event)
            else
              Some (old_average, old_event)
        )
        possible_actions
        None
    in
    match result with
    | None -> assert false
    | Some (_, event) -> event
  
