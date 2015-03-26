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
  | New_round
  | End_game


type game = Game_descr_t.game = {
  game_events: game_event list;
  current_round: round_event list
}

val write_tile_pos :
  Bi_outbuf.t -> tile_pos -> unit
  (** Output a JSON value of type {!tile_pos}. *)

val string_of_tile_pos :
  ?len:int -> tile_pos -> string
  (** Serialize a value of type {!tile_pos}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tile_pos :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tile_pos
  (** Input JSON data of type {!tile_pos}. *)

val tile_pos_of_string :
  string -> tile_pos
  (** Deserialize JSON data of type {!tile_pos}. *)

val write_tile :
  Bi_outbuf.t -> tile -> unit
  (** Output a JSON value of type {!tile}. *)

val string_of_tile :
  ?len:int -> tile -> string
  (** Serialize a value of type {!tile}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tile :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tile
  (** Input JSON data of type {!tile}. *)

val tile_of_string :
  string -> tile
  (** Deserialize JSON data of type {!tile}. *)

val write_rule_descr :
  Bi_outbuf.t -> rule_descr -> unit
  (** Output a JSON value of type {!rule_descr}. *)

val string_of_rule_descr :
  ?len:int -> rule_descr -> string
  (** Serialize a value of type {!rule_descr}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rule_descr :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rule_descr
  (** Input JSON data of type {!rule_descr}. *)

val rule_descr_of_string :
  string -> rule_descr
  (** Deserialize JSON data of type {!rule_descr}. *)

val write_round_player :
  Bi_outbuf.t -> round_player -> unit
  (** Output a JSON value of type {!round_player}. *)

val string_of_round_player :
  ?len:int -> round_player -> string
  (** Serialize a value of type {!round_player}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_round_player :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> round_player
  (** Input JSON data of type {!round_player}. *)

val round_player_of_string :
  string -> round_player
  (** Deserialize JSON data of type {!round_player}. *)

val write_round_event :
  Bi_outbuf.t -> round_event -> unit
  (** Output a JSON value of type {!round_event}. *)

val string_of_round_event :
  ?len:int -> round_event -> string
  (** Serialize a value of type {!round_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_round_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> round_event
  (** Input JSON data of type {!round_event}. *)

val round_event_of_string :
  string -> round_event
  (** Deserialize JSON data of type {!round_event}. *)

val write_ai_conf :
  Bi_outbuf.t -> ai_conf -> unit
  (** Output a JSON value of type {!ai_conf}. *)

val string_of_ai_conf :
  ?len:int -> ai_conf -> string
  (** Serialize a value of type {!ai_conf}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ai_conf :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ai_conf
  (** Input JSON data of type {!ai_conf}. *)

val ai_conf_of_string :
  string -> ai_conf
  (** Deserialize JSON data of type {!ai_conf}. *)

val write_player_kind :
  Bi_outbuf.t -> player_kind -> unit
  (** Output a JSON value of type {!player_kind}. *)

val string_of_player_kind :
  ?len:int -> player_kind -> string
  (** Serialize a value of type {!player_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_player_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> player_kind
  (** Input JSON data of type {!player_kind}. *)

val player_kind_of_string :
  string -> player_kind
  (** Deserialize JSON data of type {!player_kind}. *)

val write_player_idx :
  Bi_outbuf.t -> player_idx -> unit
  (** Output a JSON value of type {!player_idx}. *)

val string_of_player_idx :
  ?len:int -> player_idx -> string
  (** Serialize a value of type {!player_idx}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_player_idx :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> player_idx
  (** Input JSON data of type {!player_idx}. *)

val player_idx_of_string :
  string -> player_idx
  (** Deserialize JSON data of type {!player_idx}. *)

val write_player_descr :
  Bi_outbuf.t -> player_descr -> unit
  (** Output a JSON value of type {!player_descr}. *)

val string_of_player_descr :
  ?len:int -> player_descr -> string
  (** Serialize a value of type {!player_descr}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_player_descr :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> player_descr
  (** Input JSON data of type {!player_descr}. *)

val player_descr_of_string :
  string -> player_descr
  (** Deserialize JSON data of type {!player_descr}. *)

val write_game_event :
  Bi_outbuf.t -> game_event -> unit
  (** Output a JSON value of type {!game_event}. *)

val string_of_game_event :
  ?len:int -> game_event -> string
  (** Serialize a value of type {!game_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_game_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> game_event
  (** Input JSON data of type {!game_event}. *)

val game_event_of_string :
  string -> game_event
  (** Deserialize JSON data of type {!game_event}. *)

val write_game :
  Bi_outbuf.t -> game -> unit
  (** Output a JSON value of type {!game}. *)

val string_of_game :
  ?len:int -> game -> string
  (** Serialize a value of type {!game}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_game :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> game
  (** Input JSON data of type {!game}. *)

val game_of_string :
  string -> game
  (** Deserialize JSON data of type {!game}. *)

