(*Copyright (C) 2014 Denis Berthod*)

val mc_ai_with_bias:
  ?debug: bool ->
  ?irregular_hands: Tileset.irregular_hands ->
  seven_pairs: bool ->
  evaluate_game: (Game_descr.round_player -> Engine.game -> float) ->
  nb_trajectory: int ->
  Game_descr.event list ->
  float ->
  Game_descr.event
