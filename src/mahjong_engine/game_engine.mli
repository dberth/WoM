(*Copyright (C) 2015 Denis Berthod*)

type game

val build_game_engine:
  ?current_round_events: Game_descr.round_event list ->
  Game_descr.game_event list ->
  (Game_descr.game_event, game) Fsm.action_handler *
  game *
  (Game_descr.game_event, game) Fsm.state
