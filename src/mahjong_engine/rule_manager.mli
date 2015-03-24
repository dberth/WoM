(*Copyright (C) 2014 Denis Berthod*)

type rule =
  {
    irregular_hands: Tileset.irregular_hands;
    seven_pairs: bool;
    evaluate_round: (Game_descr.round_player -> Engine.round -> float);
    explain_hand_score: (Engine.round -> (string * float) list * float);
    explain_player_score: (Game_descr.round_player -> Engine.round -> hand_score: float -> string * float);
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

val rule_builder_of_name: string -> rule_builder option

val all_flags: rule_builder -> flag list

val flags_of_flag_names: rule_builder -> string list option -> flag list option 

val default_flags: rule_builder -> flag list

val rule: rule_builder -> flag list option -> rule

val default_rule: unit -> rule

val irregular_hands: rule -> Tileset.irregular_hands

val seven_pairs: rule -> bool

val evaluate_round: rule -> Game_descr.round_player -> Engine.round -> float

val explain_hand_score: rule -> Engine.round -> (string * float) list * float

val explain_player_score: rule -> Game_descr.round_player -> Engine.round -> hand_score: float -> string * float
