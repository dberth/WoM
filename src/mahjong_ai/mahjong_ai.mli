(*Copyright (C) 2014 Denis Berthod*)

val mc_ai_with_bias:
  ?debug: bool ->
  ?irregular_hands: Tileset.irregular_hands ->
  seven_pairs: bool ->
  evaluate_game: (Engine.player -> Engine.game -> float) ->
  nb_trajectory: int ->
  Engine.event list ->
  float ->
  Engine.event
