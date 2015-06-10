(*Copyright (C) 2015 Denis Berthod*)

type game

val build_game_engine:
  Game_descr.game_event list ->
  (Game_descr.game_event, game) Fsm.action_handler *
  game *
  (Game_descr.game_event, game) Fsm.state

type game_loop_callbacks =
  {
    get_rule: unit -> Game_descr.rule_descr Lwt.t;
    get_player_name: unit -> string Lwt.t;
    get_ai_player: unit -> Game_descr.player_descr Lwt.t;
    get_initial_east_seat: unit -> int Lwt.t;
    human_move: Engine.round -> Game_descr.round_event list -> Game_descr.round_event Lwt.t;
    end_round: game -> unit Lwt.t;
    new_round: game -> unit Lwt.t;
    end_game: game -> unit Lwt.t;
    on_game_event: Game_descr.game_event -> game -> unit;
  }

val one_player_game_loop: Game_descr.game_event list -> game_loop_callbacks -> unit Lwt.t
