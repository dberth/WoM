(*Copyright (C) 2015 Denis Berthod*)

open Fsm
open Game_descr

type player =
  {
    name: string;
    kind: Game_descr.player_kind;
    score: int;
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
    current_round: round_engine option;
  }

let dummy_player =
  {
    name = "";
    kind = Human;
    score = 0;
  }

let init_game =
  {
    players = Array.make 4 dummy_player;
    rule = None;
    current_round = None;
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

let on_game_start_exit event game =
  match event with
  | Set_rule rule_descr -> set_rule rule_descr game
  | _ -> assert false

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
           ~accepted_events: (fun _ -> [Init_score 0])
           (function
             | Init_score _ -> wait_for_east_seat
             | event -> raise (Irrelevant_event (event, "wait_for_score_init"))))

  and wait_for_east_seat =
    lazy (new_state
           ~accepted_events: (fun _ -> [East_seat 0])
           (function
             | East_seat _ -> wait_for_score
             | event -> raise (Irrelevant_event (event, "wait_for_east_seat"))))

  and wait_for_score =
    lazy (new_state
           ~accepted_events: (fun _ -> [Score (0, 0)])
           (function
             | Score _ -> wait_for_score_1
             | event -> raise (Irrelevant_event (event, "wait_for_score_0"))))

  and wait_for_score_1 =
    lazy (new_state
           ~accepted_events: (fun _ -> [Score (0, 0)])
           (function
             | Score _ -> wait_for_score_2
             | event -> raise (Irrelevant_event (event, "wait_for_score_1"))))

  and wait_for_score_2 =
    lazy (new_state
           ~accepted_events: (fun _ -> [Score (0, 0)])
           (function
             | Score _ -> wait_for_score_3
             | event -> raise (Irrelevant_event (event, "wait_for_score_2"))))

  and wait_for_score_3 =
    lazy (new_state
           ~accepted_events: (fun _ -> [Score (0, 0)])
           (function
             | Score _ -> wait_for_new_round
             | event -> raise (Irrelevant_event (event, "wait_for_score_3"))))

  and wait_for_new_round =
    lazy (new_state
           ~accepted_events: (fun _ -> [East_seat 0; End_game])
           (function
             | East_seat _ -> wait_for_score
             | End_game -> end_game
             | event -> raise (Irrelevant_event (event, "wait_for_new_round"))))

  and end_game =
    lazy (new_state
           (function event -> raise (Irrelevant_event (event, "end_game"))))
  in
  let action_handler =
    empty_action_handler |>
    on_exit game_start on_game_start_exit
  in
  let world, state = run action_handler init_game game_start game_events in
  action_handler, world, state
