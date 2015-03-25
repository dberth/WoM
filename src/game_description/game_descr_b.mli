(* Auto-generated from "game_descr.atd" *)


(** A position in the initial array. *)
type tile_pos = Game_descr_t.tile_pos

type tile = Game_descr_t.tile

type rule_descr = Game_descr_t.rule_descr = {
  name: string;
  flags: string list option
}

(** Player 0 is east and so on. *)
type round_player = Game_descr_t.round_player

type round_event = Game_descr_t.round_event = 
    Init of tile option Ag_util.ocaml_array
  | Wall_breaker_roll of int
  | Break_wall_roll of int
  | Deal
  | Draw of round_player
  | Discard of (round_player * tile_pos)
  | Mahjong of round_player
  | Concealed_kong of (round_player * tile_pos list)
  | Small_kong of (round_player * tile_pos)
  | Chow of (round_player * tile_pos list)
  | Pong of (round_player * tile_pos list)
  | Kong of (round_player * tile_pos list)
  | No_action of round_player


type ai_conf = Game_descr_t.ai_conf = { name: string; force: int }

type player_kind = Game_descr_t.player_kind =  Human | AI of ai_conf 

type player_idx = Game_descr_t.player_idx

type player_descr = Game_descr_t.player_descr = {
  name: string;
  kind: player_kind
}

type game_event = Game_descr_t.game_event = 
    Set_rule of rule_descr
  | Player of player_descr
  | East_seat of player_idx
  | Init_score of float
  | Round_event of round_event
  | End_round
  | End_game


type game = Game_descr_t.game = {
  game_events: game_event list;
  current_round: round_event list
}

(* Writers for type tile_pos *)

val tile_pos_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!tile_pos}.
      Readers may support more than just this tag. *)

val write_untagged_tile_pos :
  Bi_outbuf.t -> tile_pos -> unit
  (** Output an untagged biniou value of type {!tile_pos}. *)

val write_tile_pos :
  Bi_outbuf.t -> tile_pos -> unit
  (** Output a biniou value of type {!tile_pos}. *)

val string_of_tile_pos :
  ?len:int -> tile_pos -> string
  (** Serialize a value of type {!tile_pos} into
      a biniou string. *)

(* Readers for type tile_pos *)

val get_tile_pos_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> tile_pos)
  (** Return a function that reads an untagged
      biniou value of type {!tile_pos}. *)

val read_tile_pos :
  Bi_inbuf.t -> tile_pos
  (** Input a tagged biniou value of type {!tile_pos}. *)

val tile_pos_of_string :
  ?pos:int -> string -> tile_pos
  (** Deserialize a biniou value of type {!tile_pos}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type tile *)

val tile_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!tile}.
      Readers may support more than just this tag. *)

val write_untagged_tile :
  Bi_outbuf.t -> tile -> unit
  (** Output an untagged biniou value of type {!tile}. *)

val write_tile :
  Bi_outbuf.t -> tile -> unit
  (** Output a biniou value of type {!tile}. *)

val string_of_tile :
  ?len:int -> tile -> string
  (** Serialize a value of type {!tile} into
      a biniou string. *)

(* Readers for type tile *)

val get_tile_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> tile)
  (** Return a function that reads an untagged
      biniou value of type {!tile}. *)

val read_tile :
  Bi_inbuf.t -> tile
  (** Input a tagged biniou value of type {!tile}. *)

val tile_of_string :
  ?pos:int -> string -> tile
  (** Deserialize a biniou value of type {!tile}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type rule_descr *)

val rule_descr_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!rule_descr}.
      Readers may support more than just this tag. *)

val write_untagged_rule_descr :
  Bi_outbuf.t -> rule_descr -> unit
  (** Output an untagged biniou value of type {!rule_descr}. *)

val write_rule_descr :
  Bi_outbuf.t -> rule_descr -> unit
  (** Output a biniou value of type {!rule_descr}. *)

val string_of_rule_descr :
  ?len:int -> rule_descr -> string
  (** Serialize a value of type {!rule_descr} into
      a biniou string. *)

(* Readers for type rule_descr *)

val get_rule_descr_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> rule_descr)
  (** Return a function that reads an untagged
      biniou value of type {!rule_descr}. *)

val read_rule_descr :
  Bi_inbuf.t -> rule_descr
  (** Input a tagged biniou value of type {!rule_descr}. *)

val rule_descr_of_string :
  ?pos:int -> string -> rule_descr
  (** Deserialize a biniou value of type {!rule_descr}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type round_player *)

val round_player_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!round_player}.
      Readers may support more than just this tag. *)

val write_untagged_round_player :
  Bi_outbuf.t -> round_player -> unit
  (** Output an untagged biniou value of type {!round_player}. *)

val write_round_player :
  Bi_outbuf.t -> round_player -> unit
  (** Output a biniou value of type {!round_player}. *)

val string_of_round_player :
  ?len:int -> round_player -> string
  (** Serialize a value of type {!round_player} into
      a biniou string. *)

(* Readers for type round_player *)

val get_round_player_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> round_player)
  (** Return a function that reads an untagged
      biniou value of type {!round_player}. *)

val read_round_player :
  Bi_inbuf.t -> round_player
  (** Input a tagged biniou value of type {!round_player}. *)

val round_player_of_string :
  ?pos:int -> string -> round_player
  (** Deserialize a biniou value of type {!round_player}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type round_event *)

val round_event_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!round_event}.
      Readers may support more than just this tag. *)

val write_untagged_round_event :
  Bi_outbuf.t -> round_event -> unit
  (** Output an untagged biniou value of type {!round_event}. *)

val write_round_event :
  Bi_outbuf.t -> round_event -> unit
  (** Output a biniou value of type {!round_event}. *)

val string_of_round_event :
  ?len:int -> round_event -> string
  (** Serialize a value of type {!round_event} into
      a biniou string. *)

(* Readers for type round_event *)

val get_round_event_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> round_event)
  (** Return a function that reads an untagged
      biniou value of type {!round_event}. *)

val read_round_event :
  Bi_inbuf.t -> round_event
  (** Input a tagged biniou value of type {!round_event}. *)

val round_event_of_string :
  ?pos:int -> string -> round_event
  (** Deserialize a biniou value of type {!round_event}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type ai_conf *)

val ai_conf_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!ai_conf}.
      Readers may support more than just this tag. *)

val write_untagged_ai_conf :
  Bi_outbuf.t -> ai_conf -> unit
  (** Output an untagged biniou value of type {!ai_conf}. *)

val write_ai_conf :
  Bi_outbuf.t -> ai_conf -> unit
  (** Output a biniou value of type {!ai_conf}. *)

val string_of_ai_conf :
  ?len:int -> ai_conf -> string
  (** Serialize a value of type {!ai_conf} into
      a biniou string. *)

(* Readers for type ai_conf *)

val get_ai_conf_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> ai_conf)
  (** Return a function that reads an untagged
      biniou value of type {!ai_conf}. *)

val read_ai_conf :
  Bi_inbuf.t -> ai_conf
  (** Input a tagged biniou value of type {!ai_conf}. *)

val ai_conf_of_string :
  ?pos:int -> string -> ai_conf
  (** Deserialize a biniou value of type {!ai_conf}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type player_kind *)

val player_kind_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!player_kind}.
      Readers may support more than just this tag. *)

val write_untagged_player_kind :
  Bi_outbuf.t -> player_kind -> unit
  (** Output an untagged biniou value of type {!player_kind}. *)

val write_player_kind :
  Bi_outbuf.t -> player_kind -> unit
  (** Output a biniou value of type {!player_kind}. *)

val string_of_player_kind :
  ?len:int -> player_kind -> string
  (** Serialize a value of type {!player_kind} into
      a biniou string. *)

(* Readers for type player_kind *)

val get_player_kind_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> player_kind)
  (** Return a function that reads an untagged
      biniou value of type {!player_kind}. *)

val read_player_kind :
  Bi_inbuf.t -> player_kind
  (** Input a tagged biniou value of type {!player_kind}. *)

val player_kind_of_string :
  ?pos:int -> string -> player_kind
  (** Deserialize a biniou value of type {!player_kind}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type player_idx *)

val player_idx_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!player_idx}.
      Readers may support more than just this tag. *)

val write_untagged_player_idx :
  Bi_outbuf.t -> player_idx -> unit
  (** Output an untagged biniou value of type {!player_idx}. *)

val write_player_idx :
  Bi_outbuf.t -> player_idx -> unit
  (** Output a biniou value of type {!player_idx}. *)

val string_of_player_idx :
  ?len:int -> player_idx -> string
  (** Serialize a value of type {!player_idx} into
      a biniou string. *)

(* Readers for type player_idx *)

val get_player_idx_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> player_idx)
  (** Return a function that reads an untagged
      biniou value of type {!player_idx}. *)

val read_player_idx :
  Bi_inbuf.t -> player_idx
  (** Input a tagged biniou value of type {!player_idx}. *)

val player_idx_of_string :
  ?pos:int -> string -> player_idx
  (** Deserialize a biniou value of type {!player_idx}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type player_descr *)

val player_descr_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!player_descr}.
      Readers may support more than just this tag. *)

val write_untagged_player_descr :
  Bi_outbuf.t -> player_descr -> unit
  (** Output an untagged biniou value of type {!player_descr}. *)

val write_player_descr :
  Bi_outbuf.t -> player_descr -> unit
  (** Output a biniou value of type {!player_descr}. *)

val string_of_player_descr :
  ?len:int -> player_descr -> string
  (** Serialize a value of type {!player_descr} into
      a biniou string. *)

(* Readers for type player_descr *)

val get_player_descr_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> player_descr)
  (** Return a function that reads an untagged
      biniou value of type {!player_descr}. *)

val read_player_descr :
  Bi_inbuf.t -> player_descr
  (** Input a tagged biniou value of type {!player_descr}. *)

val player_descr_of_string :
  ?pos:int -> string -> player_descr
  (** Deserialize a biniou value of type {!player_descr}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type game_event *)

val game_event_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!game_event}.
      Readers may support more than just this tag. *)

val write_untagged_game_event :
  Bi_outbuf.t -> game_event -> unit
  (** Output an untagged biniou value of type {!game_event}. *)

val write_game_event :
  Bi_outbuf.t -> game_event -> unit
  (** Output a biniou value of type {!game_event}. *)

val string_of_game_event :
  ?len:int -> game_event -> string
  (** Serialize a value of type {!game_event} into
      a biniou string. *)

(* Readers for type game_event *)

val get_game_event_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> game_event)
  (** Return a function that reads an untagged
      biniou value of type {!game_event}. *)

val read_game_event :
  Bi_inbuf.t -> game_event
  (** Input a tagged biniou value of type {!game_event}. *)

val game_event_of_string :
  ?pos:int -> string -> game_event
  (** Deserialize a biniou value of type {!game_event}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

(* Writers for type game *)

val game_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!game}.
      Readers may support more than just this tag. *)

val write_untagged_game :
  Bi_outbuf.t -> game -> unit
  (** Output an untagged biniou value of type {!game}. *)

val write_game :
  Bi_outbuf.t -> game -> unit
  (** Output a biniou value of type {!game}. *)

val string_of_game :
  ?len:int -> game -> string
  (** Serialize a value of type {!game} into
      a biniou string. *)

(* Readers for type game *)

val get_game_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> game)
  (** Return a function that reads an untagged
      biniou value of type {!game}. *)

val read_game :
  Bi_inbuf.t -> game
  (** Input a tagged biniou value of type {!game}. *)

val game_of_string :
  ?pos:int -> string -> game
  (** Deserialize a biniou value of type {!game}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

