(*Copyright (C) 2015 Denis Berthod*)

type game

val build_game_engine:
  Game_descr.game_event list ->
  (Game_descr.game_event, game) Fsm.action_handler *
  game *
  (Game_descr.game_event, game) Fsm.state

type game_loop_callbacks =
  {
    get_rule: unit -> Game_descr.rule_descr;
    get_player_name: unit -> string;
    get_ai_player: unit -> Game_descr.player_descr;
    get_initial_east_seat: unit -> int;
    human_move: Engine.round -> Game_descr.round_event list -> Game_descr.round_event;
    end_round: game -> unit;
    new_round: game -> unit;
    end_game: game -> unit
  }

val one_player_game_loop: Game_descr.game_event list -> game_loop_callbacks -> unit
