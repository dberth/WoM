(*Copyright (C) 2015 Denis Berthod*)

open Fsm
open Game_descr

type player =
  {
    name: string;
    kind: Game_descr.player_kind;
    score: float;
  }

type round_engine =
  {
    action_handler: (Game_descr.round_event, Engine.round) Fsm.action_handler;
    round: Engine.round;
    state: (Game_descr.round_event, Engine.round) Fsm.state;
  }

type game =
  {
    players: player array;
    rule: Rule_manager.rule option;
    east_seat: int;
    current_round: round_engine option;
    round_finished: bool;
    nb_rounds: int;
  }

let dummy_player =
  {
    name = "";
    kind = Human;
    score = 0.;
  }

let init_game =
  {
    players = Array.make 4 dummy_player;
    rule = None;
    east_seat = 0;
    current_round = None;
    round_finished = false;
    nb_rounds = 0;
  }


exception Irrelevant_event of (Game_descr.game_event * string)

let dummy_player_descr =
  {
    name = "";
    kind = Human;
  }

let set_rule {name; flags} game =
  match Rule_manager.rule_builder_of_name name with
  | None -> failwith (Printf.sprintf "Unknown rule set: %s" name)
  | Some rule_builder ->
    let flags = Rule_manager.flags_of_flag_names rule_builder flags in
    let rule = Rule_manager.rule rule_builder flags in
    {game with rule = Some rule}

let player_of_player_descr {Game_descr.name; kind} =
  {
    name;
    kind;
    score = 0.;
  }

let on_game_start_exit event game =
  match event with
  | Set_rule rule_descr -> set_rule rule_descr game
  | _ -> assert false

let on_wait_for_player_exit player_idx event game =
  match event with
  | Player player_descr ->
    game.players.(player_idx) <- player_of_player_descr player_descr;
    game
  | _ -> assert false

let on_wait_for_score_init_exit event game =
  match event with
  | Init_score score ->
    for i = 0 to 3 do
      let player = game.players.(i) in
      game.players.(i) <- {player with score}
    done;
    game
  | _ -> assert false

let on_wait_for_east_seat_exit event game =
  match event with
  | East_seat player_idx -> {game with east_seat = player_idx}
  | _ -> assert false

let on_round_entry _ ({current_round; rule; _} as game) =
  match current_round with
  | Some _ -> game
  | None ->
    match rule with
    | None -> assert false
    | Some rule ->
      let seven_pairs = Rule_manager.seven_pairs rule in
      let irregular_hands = Rule_manager.irregular_hands rule in
      let action_handler, round, state =
        Engine.build_engine
          ~seven_pairs
          ~irregular_hands
          []
      in
      {game with
       current_round = Some {action_handler; round; state};
       round_finished = false
      }

let on_round_exit event ({current_round; players; east_seat; rule; _} as game) =
  match current_round with
  | None -> assert false
  | Some {action_handler; round; state} ->
    match event with
    | Round_event round_event ->
      let round, state =
        Fsm.run ~with_history: true action_handler round (lazy state) [round_event]
      in
      {game with current_round = Some {action_handler; round; state}}

    | End_round ->
      begin match rule with
      | None -> assert false
      | Some rule ->
        for i =0 to 3 do
          let player = players.(i) in
          let round_player = (i + (4 - east_seat)) mod 4 in
          let score = player.score +. Rule_manager.evaluate_round rule round_player round in
          players.(i) <- {player with score} 
        done;
        {game with round_finished = true; nb_rounds = game.nb_rounds + 1}
      end
    | _ -> assert false

let on_wait_for_new_round_exit event ({east_seat; _} as game) =
  match event with
  | New_round ->
    let east_seat = (east_seat + 1) mod 4 in (*TODO should depend on the rule*)
    {game with east_seat; current_round = None}
  | End_game -> game
  | _ -> assert false

let round_accepted_events {current_round; _} =
  match current_round with
  | None -> [Round_event (Init [||])]
  | Some {state; round; _} ->
    match Engine.finished round with
    | Some _ -> [End_round]
    | None ->
      List.map (fun x -> Round_event x) (Fsm.accepted_events round state)

let new_round_accepted_events {nb_rounds; rule; _} =
  match rule with
  | None -> assert false
  | Some rule ->
    if Rule_manager.game_finished rule nb_rounds then
      [End_game]
    else
      [New_round]

let build_game_engine ?current_round_events game_events =
  let rec game_start =
    lazy (new_state
           ~accepted_events: (fun _ -> [Set_rule {name = ""; flags = None}])
            (function
              | Set_rule _ -> wait_for_player_0
              | event -> raise (Irrelevant_event(event, "game_start"))))

  and wait_for_player_0 =
    lazy (new_state
           ~accepted_events: (fun _ -> [Player dummy_player_descr])
           (function
             | Player _ -> wait_for_player_1
             | event -> raise (Irrelevant_event (event, "wait_for_player_0"))))

  and wait_for_player_1 =
    lazy (new_state
           ~accepted_events: (fun _ -> [Player dummy_player_descr])
           (function
             | Player _ -> wait_for_player_2
             | event -> raise (Irrelevant_event (event, "wait_for_plauer_1"))))

  and wait_for_player_2 =
    lazy (new_state
           ~accepted_events: (fun _ -> [Player dummy_player_descr])
           (function
             | Player _ -> wait_for_player_3
             | event -> raise (Irrelevant_event (event, "wait_for_player_2"))))

  and wait_for_player_3 =
    lazy (new_state
           ~accepted_events: (fun _ -> [Player dummy_player_descr])
           (function
             | Player _ -> wait_for_score_init
             | event -> raise (Irrelevant_event (event, "wait_for_player_3"))))

  and wait_for_score_init =
    lazy (new_state
           ~accepted_events: (fun _ -> [Init_score 0.])
           (function
             | Init_score _ -> wait_for_east_seat
             | event -> raise (Irrelevant_event (event, "wait_for_score_init"))))

  and wait_for_east_seat =
    lazy (new_state
           ~accepted_events: (fun _ -> [East_seat 0])
           (function
             | East_seat _ -> round
             | event -> raise (Irrelevant_event (event, "wait_for_east_seat"))))
      
  and round =
    lazy (new_state
           ~accepted_events: round_accepted_events
           (function
             | Round_event _ -> round
             | End_round -> wait_for_new_round
             | event -> raise (Irrelevant_event (event, "round"))))

  and wait_for_new_round =
    lazy (new_state
           ~accepted_events: new_round_accepted_events
           (function
             | New_round -> round
             | End_game -> end_game
             | event -> raise (Irrelevant_event (event, "wait_for_new_round"))))

  and end_game =
    lazy (new_state
           (function event -> raise (Irrelevant_event (event, "end_game"))))
  in
  let action_handler =
    empty_action_handler |>
    on_exit game_start on_game_start_exit |>
    on_exit wait_for_player_0 (on_wait_for_player_exit 0) |>
    on_exit wait_for_player_0 (on_wait_for_player_exit 1) |>
    on_exit wait_for_player_0 (on_wait_for_player_exit 2) |>
    on_exit wait_for_player_0 (on_wait_for_player_exit 3) |>
    on_exit wait_for_score_init on_wait_for_score_init_exit |>
    on_exit wait_for_east_seat on_wait_for_east_seat_exit |>
    on_entry round on_round_entry |>
    on_exit round on_round_exit |>
    on_exit wait_for_new_round on_wait_for_new_round_exit
  in
  let world, state = run action_handler init_game game_start game_events in
  action_handler, world, state
