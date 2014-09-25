(*Copyright (C) 2014 Denis Berthod*)

type rule =
  {
    irregular_hands: Tileset.irregular_hands;
    seven_pairs: bool;
    evaluate_game: (Engine.player -> Engine.game -> float);
    explain_hand_score: (Engine.game -> (string * float) list * float);
    explain_player_score: (Engine.player -> Engine.game -> hand_score: float -> string * float);
  }

type flag

val flag: string -> flag

val name_of_flag: flag -> string

type rule_builder

val register_rule_builder:
  is_default: bool ->
  flags: flag list ->
  default_flags: flag list ->
  build_rule: ((flag -> bool) -> rule) ->
  string -> unit

val all_rule_builders: unit -> rule_builder list

val all_flags: rule_builder -> flag list

val default_flags: rule_builder -> flag list

val set_rule: rule_builder -> flag list option -> unit

val set_default_rule: unit -> unit

val irregular_hands: unit -> Tileset.irregular_hands

val seven_pairs: unit -> bool

val evaluate_game: Engine.player -> Engine.game -> float
