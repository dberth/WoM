(*Copyright (C) 2014 Denis Berthod*)

val mc_ai_with_bias:
  ?debug: bool ->
  ?irregular_hands: Tileset.irregular_hands ->
  seven_pairs: bool ->
  evaluate_round: (Game_descr.round_player -> Engine.round -> float) ->
  nb_trajectory: int ->
  Game_descr.round_event list ->
  float ->
  Game_descr.round_event
