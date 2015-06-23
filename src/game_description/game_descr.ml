(*Copyright (C) 2015 Denis Berthod*)

type round_player = Game_descr_t.round_player

type tile_pos = Game_descr_t.tile_pos

type round_event = Game_descr_t.round_event =
  | Init of Tileset.tile option array
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

type player_idx = Game_descr_t.player_idx

type ai_conf = Game_descr_t.ai_conf =
  {
    name: string;
    force: int;
  }

type player_kind = Game_descr_t.player_kind =
  | Human
  | AI of ai_conf

type player_descr = Game_descr_t.player_descr =
  {
    name: string;
    kind: player_kind;
  }

type rule_descr = Game_descr_t.rule_descr =
  {
    name: string;
    flags: string list option;
  }

type game_event = Game_descr_t.game_event =
  | Set_rule of rule_descr
  | Player of player_descr
  | East_seat of player_idx
  | Init_score of float
  | Round_event of round_event
  | End_round
  | New_round
  | End_game
  

type game = Game_descr_t.game =
  {
    game_events: game_event list;
    current_round: round_event list;
 }

let dump ?(json = false) game path =
  let oc = open_out path in
  let outbuf = Bi_outbuf.create_channel_writer oc in
  let write_game =
    if json then Game_descr_j.write_game else Game_descr_b.write_game
  in
  write_game outbuf game;
  Bi_outbuf.flush_channel_writer outbuf;
  close_out oc

let restore path =
  let ic = open_in path in
  let inbuf = Bi_inbuf.from_channel ic in
  match Game_descr_b.read_game inbuf with
  | game -> close_in ic; game
  | exception exn ->
    close_in ic;
    let ic = open_in path in
    let lexer_state = Yojson.init_lexer () in
    let lexbuf = Lexing.from_channel ic in
    match Game_descr_j.read_game lexer_state lexbuf with
    | game -> close_in ic; game
    | exception _ -> close_in ic; raise exn

let string_of_round_event event = Game_descr_j.string_of_round_event event

let string_of_game_event event = Game_descr_j.string_of_game_event event

let string_of_tile tile = Game_descr_j.string_of_tile tile
