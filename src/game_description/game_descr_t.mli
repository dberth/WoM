(* Auto-generated from "game_descr.atd" *)


(** A position in the initial array. *)
type tile_pos = int

type tile = Tileset.tile

type rule_descr = { name: string; flags: string list option }

(** Player 0 is east and so on. *)
type round_player = int

type round_event = 
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


type ai_conf = { name: string; force: int }

type player_kind =  Human | AI of ai_conf 

type player_idx = int

type player_descr = { name: string; kind: player_kind }

type game_event = 
    Set_rule of rule_descr
  | Player of player_descr
  | East_seat of player_idx
  | Init_score of int
  | Round_event of round_event
  | End_round
  | End_game


type game = { game_events: game_event list; current_round: round_event list }
