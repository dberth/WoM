(*Copyright (C) 2015 Denis Berthod*)

type game

val east_seat: game -> int

val wall_start: game -> int option

val last_tile: game -> int option

val discarded_tile: game -> Tileset.tile option

val discard_player: game -> int option

val player_name: game -> int -> string

val hand: game -> int -> Tileset.tileset option

val exposed: game -> int -> (Tileset.tileset * bool (*is_concealed*)) list

val discarded: game -> int -> Tileset.tile list

val tile_of_tile_pos: game -> int -> Tileset.tile option

val last_drawn_tile: game -> int -> Tileset.tile option

val is_draw_game: game -> bool

val explain_hand_score: game -> (string * float) list * float

val explain_player_score: game -> int -> float -> string * float

val current_player_name: game -> string option

val player_score: game -> int -> float

val nb_rounds: game -> int

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
    wall_breaker_roll: unit -> int Lwt.t;
    break_wall_roll: unit -> int Lwt.t;
    human_move: game -> Game_descr.round_event list -> Game_descr.round_event Lwt.t;
    end_round: game -> unit Lwt.t;
    new_round: game -> unit Lwt.t;
    end_game: game -> unit Lwt.t;
    on_game_event: Game_descr.game_event -> game -> unit Lwt.t;
  }

val one_player_game_loop: Game_descr.game_event list -> game_loop_callbacks -> unit Lwt.t
