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

let write_tile_pos = (
  Yojson.Safe.write_int
)
let string_of_tile_pos ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tile_pos ob x;
  Bi_outbuf.contents ob
let read_tile_pos = (
  Ag_oj_run.read_int
)
let tile_pos_of_string s =
  read_tile_pos (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  fun ob x -> (
    let x = ( Tileset.string_of_tile ) x in (
      Yojson.Safe.write_string
    ) ob x)
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  fun p lb ->
    let x = (
      Ag_oj_run.read_string
    ) p lb in
    ( Tileset.tile_of_string ) x
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tile = (
  write__1
)
let string_of_tile ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tile ob x;
  Bi_outbuf.contents ob
let read_tile = (
  read__1
)
let tile_of_string s =
  read_tile (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Ag_oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  Ag_oj_run.read_list (
    Ag_oj_run.read_string
  )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Ag_oj_run.write_option (
    write__5
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read__5
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__5
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_rule_descr : _ -> rule_descr -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"flags\":";
    (
      write__6
    )
      ob x.flags;
    Bi_outbuf.add_char ob '}';
)
let string_of_rule_descr ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_rule_descr ob x;
  Bi_outbuf.contents ob
let read_rule_descr = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (Obj.magic 0.0) in
    let field_flags = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_flags := (
              (
                read__6
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_flags := (
                (
                  read__6
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "name"; "flags" |];
        (
          {
            name = !field_name;
            flags = !field_flags;
          }
         : rule_descr)
      )
)
let rule_descr_of_string s =
  read_rule_descr (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_round_player = (
  Yojson.Safe.write_int
)
let string_of_round_player ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_round_player ob x;
  Bi_outbuf.contents ob
let read_round_player = (
  Ag_oj_run.read_int
)
let round_player_of_string s =
  read_round_player (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Ag_oj_run.write_list (
    write_tile_pos
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Ag_oj_run.read_list (
    read_tile_pos
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_option (
    write_tile
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_tile
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_tile
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_array (
    write__2
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Ag_oj_run.read_array (
    read__2
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_round_event : _ -> round_event -> _ = (
  fun ob sum ->
    match sum with
      | Init x ->
        Bi_outbuf.add_string ob "<\"Init\":";
        (
          write__3
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Wall_breaker_roll x ->
        Bi_outbuf.add_string ob "<\"Wall_breaker_roll\":";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Break_wall_roll x ->
        Bi_outbuf.add_string ob "<\"Break_wall_roll\":";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Deal -> Bi_outbuf.add_string ob "<\"Deal\">"
      | Draw x ->
        Bi_outbuf.add_string ob "<\"Draw\":";
        (
          write_round_player
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Discard x ->
        Bi_outbuf.add_string ob "<\"Discard\":";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '(';
            (let x, _ = x in
            (
              write_round_player
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              write_tile_pos
            ) ob x
            );
            Bi_outbuf.add_char ob ')';
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Mahjong x ->
        Bi_outbuf.add_string ob "<\"Mahjong\":";
        (
          write_round_player
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Concealed_kong x ->
        Bi_outbuf.add_string ob "<\"Concealed_kong\":";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '(';
            (let x, _ = x in
            (
              write_round_player
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              write__4
            ) ob x
            );
            Bi_outbuf.add_char ob ')';
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Small_kong x ->
        Bi_outbuf.add_string ob "<\"Small_kong\":";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '(';
            (let x, _ = x in
            (
              write_round_player
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              write_tile_pos
            ) ob x
            );
            Bi_outbuf.add_char ob ')';
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Chow x ->
        Bi_outbuf.add_string ob "<\"Chow\":";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '(';
            (let x, _ = x in
            (
              write_round_player
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              write__4
            ) ob x
            );
            Bi_outbuf.add_char ob ')';
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Pong x ->
        Bi_outbuf.add_string ob "<\"Pong\":";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '(';
            (let x, _ = x in
            (
              write_round_player
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              write__4
            ) ob x
            );
            Bi_outbuf.add_char ob ')';
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Kong x ->
        Bi_outbuf.add_string ob "<\"Kong\":";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '(';
            (let x, _ = x in
            (
              write_round_player
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              write__4
            ) ob x
            );
            Bi_outbuf.add_char ob ')';
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | No_action x ->
        Bi_outbuf.add_string ob "<\"No_action\":";
        (
          write_round_player
        ) ob x;
        Bi_outbuf.add_char ob '>'
)
let string_of_round_event ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_round_event ob x;
  Bi_outbuf.contents ob
let read_round_event = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 4 -> (
                      match String.unsafe_get s pos with
                        | 'C' -> (
                            if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'w' then (
                              9
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'D' -> (
                            match String.unsafe_get s (pos+1) with
                              | 'e' -> (
                                  if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' then (
                                    3
                                  )
                                  else (
                                    raise (Exit)
                                  )
                                )
                              | 'r' -> (
                                  if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'w' then (
                                    4
                                  )
                                  else (
                                    raise (Exit)
                                  )
                                )
                              | _ -> (
                                  raise (Exit)
                                )
                          )
                        | 'I' -> (
                            if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' then (
                              0
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'K' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' then (
                              11
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'P' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' then (
                              10
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 7 -> (
                      match String.unsafe_get s pos with
                        | 'D' -> (
                            if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'd' then (
                              5
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'M' -> (
                            if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'j' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' then (
                              6
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 9 -> (
                      if String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' then (
                        12
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 10 -> (
                      if String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'k' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'g' then (
                        8
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 14 -> (
                      if String.unsafe_get s pos = 'C' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 'g' then (
                        7
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 15 -> (
                      if String.unsafe_get s pos = 'B' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'k' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'w' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' then (
                        2
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 17 -> (
                      if String.unsafe_get s pos = 'W' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'k' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 'l' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read__3
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Init x : round_event)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  Ag_oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Wall_breaker_roll x : round_event)
            | 2 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  Ag_oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Break_wall_roll x : round_event)
            | 3 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Deal : round_event)
            | 4 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_round_player
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Draw x : round_event)
            | 5 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read_tile_pos
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Discard x : round_event)
            | 6 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_round_player
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Mahjong x : round_event)
            | 7 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Concealed_kong x : round_event)
            | 8 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read_tile_pos
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Small_kong x : round_event)
            | 9 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Chow x : round_event)
            | 10 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Pong x : round_event)
            | 11 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Kong x : round_event)
            | 12 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_round_player
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (No_action x : round_event)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'D' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (Deal : round_event)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 4 -> (
                      match String.unsafe_get s pos with
                        | 'C' -> (
                            if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'w' then (
                              8
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'D' -> (
                            if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'w' then (
                              3
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'I' -> (
                            if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' then (
                              0
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'K' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' then (
                              10
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'P' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' then (
                              9
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 7 -> (
                      match String.unsafe_get s pos with
                        | 'D' -> (
                            if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'd' then (
                              4
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'M' -> (
                            if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'j' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' then (
                              5
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 9 -> (
                      if String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' then (
                        11
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 10 -> (
                      if String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'k' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'g' then (
                        7
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 14 -> (
                      if String.unsafe_get s pos = 'C' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 'g' then (
                        6
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 15 -> (
                      if String.unsafe_get s pos = 'B' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'k' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'w' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' then (
                        2
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 17 -> (
                      if String.unsafe_get s pos = 'W' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'k' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 'l' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__3
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Init x : round_event)
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Ag_oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Wall_breaker_roll x : round_event)
            | 2 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Ag_oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Break_wall_roll x : round_event)
            | 3 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_round_player
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Draw x : round_event)
            | 4 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read_tile_pos
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Discard x : round_event)
            | 5 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_round_player
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Mahjong x : round_event)
            | 6 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Concealed_kong x : round_event)
            | 7 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read_tile_pos
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Small_kong x : round_event)
            | 8 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Chow x : round_event)
            | 9 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Pong x : round_event)
            | 10 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_round_player
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read__4
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Kong x : round_event)
            | 11 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_round_player
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (No_action x : round_event)
            | _ -> (
                assert false
              )
        )
)
let round_event_of_string s =
  read_round_event (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_ai_conf : _ -> ai_conf -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"force\":";
    (
      Yojson.Safe.write_int
    )
      ob x.force;
    Bi_outbuf.add_char ob '}';
)
let string_of_ai_conf ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ai_conf ob x;
  Bi_outbuf.contents ob
let read_ai_conf = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (Obj.magic 0.0) in
    let field_force = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_force := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_force := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "name"; "force" |];
        (
          {
            name = !field_name;
            force = !field_force;
          }
         : ai_conf)
      )
)
let ai_conf_of_string s =
  read_ai_conf (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_player_kind : _ -> player_kind -> _ = (
  fun ob sum ->
    match sum with
      | Human -> Bi_outbuf.add_string ob "<\"Human\">"
      | AI x ->
        Bi_outbuf.add_string ob "<\"AI\":";
        (
          write_ai_conf
        ) ob x;
        Bi_outbuf.add_char ob '>'
)
let string_of_player_kind ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player_kind ob x;
  Bi_outbuf.contents ob
let read_player_kind = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 2 -> (
                      if String.unsafe_get s pos = 'A' && String.unsafe_get s (pos+1) = 'I' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 5 -> (
                      if String.unsafe_get s pos = 'H' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'n' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Human : player_kind)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_ai_conf
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (AI x : player_kind)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 5 && String.unsafe_get s pos = 'H' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'n' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (Human : player_kind)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 2 && String.unsafe_get s pos = 'A' && String.unsafe_get s (pos+1) = 'I' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_ai_conf
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (AI x : player_kind)
            | _ -> (
                assert false
              )
        )
)
let player_kind_of_string s =
  read_player_kind (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_player_idx = (
  Yojson.Safe.write_int
)
let string_of_player_idx ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player_idx ob x;
  Bi_outbuf.contents ob
let read_player_idx = (
  Ag_oj_run.read_int
)
let player_idx_of_string s =
  read_player_idx (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_player_descr : _ -> player_descr -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"kind\":";
    (
      write_player_kind
    )
      ob x.kind;
    Bi_outbuf.add_char ob '}';
)
let string_of_player_descr ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player_descr ob x;
  Bi_outbuf.contents ob
let read_player_descr = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (Obj.magic 0.0) in
    let field_kind = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 4 then (
            match String.unsafe_get s pos with
              | 'k' -> (
                  if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 'n' -> (
                  if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_kind := (
              (
                read_player_kind
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 4 then (
              match String.unsafe_get s pos with
                | 'k' -> (
                    if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | 'n' -> (
                    if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_kind := (
                (
                  read_player_kind
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "name"; "kind" |];
        (
          {
            name = !field_name;
            kind = !field_kind;
          }
         : player_descr)
      )
)
let player_descr_of_string s =
  read_player_descr (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_game_event : _ -> game_event -> _ = (
  fun ob sum ->
    match sum with
      | Set_rule x ->
        Bi_outbuf.add_string ob "<\"Set_rule\":";
        (
          write_rule_descr
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Player x ->
        Bi_outbuf.add_string ob "<\"Player\":";
        (
          write_player_descr
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | East_seat x ->
        Bi_outbuf.add_string ob "<\"East_seat\":";
        (
          write_player_idx
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Init_score x ->
        Bi_outbuf.add_string ob "<\"Init_score\":";
        (
          Yojson.Safe.write_float
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | Round_event x ->
        Bi_outbuf.add_string ob "<\"Round_event\":";
        (
          write_round_event
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | End_round -> Bi_outbuf.add_string ob "<\"End_round\">"
      | New_round -> Bi_outbuf.add_string ob "<\"New_round\">"
      | End_game -> Bi_outbuf.add_string ob "<\"End_game\">"
)
let string_of_game_event ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_game_event ob x;
  Bi_outbuf.contents ob
let read_game_event = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 6 -> (
                      if String.unsafe_get s pos = 'P' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'y' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 8 -> (
                      match String.unsafe_get s pos with
                        | 'E' -> (
                            if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'e' then (
                              7
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'S' -> (
                            if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'e' then (
                              0
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 9 -> (
                      match String.unsafe_get s pos with
                        | 'E' -> (
                            match String.unsafe_get s (pos+1) with
                              | 'a' -> (
                                  if String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 't' then (
                                    2
                                  )
                                  else (
                                    raise (Exit)
                                  )
                                )
                              | 'n' -> (
                                  if String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'd' then (
                                    5
                                  )
                                  else (
                                    raise (Exit)
                                  )
                                )
                              | _ -> (
                                  raise (Exit)
                                )
                          )
                        | 'N' -> (
                            if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'd' then (
                              6
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 10 -> (
                      if String.unsafe_get s pos = 'I' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' then (
                        3
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 11 -> (
                      if String.unsafe_get s pos = 'R' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'v' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 't' then (
                        4
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_rule_descr
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Set_rule x : game_event)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_player_descr
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Player x : game_event)
            | 2 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_player_idx
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (East_seat x : game_event)
            | 3 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  Ag_oj_run.read_number
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Init_score x : game_event)
            | 4 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_round_event
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Round_event x : game_event)
            | 5 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (End_round : game_event)
            | 6 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (New_round : game_event)
            | 7 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (End_game : game_event)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 8 -> (
                      if String.unsafe_get s pos = 'E' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'e' then (
                        2
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 9 -> (
                      match String.unsafe_get s pos with
                        | 'E' -> (
                            if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'd' then (
                              0
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'N' -> (
                            if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'd' then (
                              1
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (End_round : game_event)
            | 1 ->
              (New_round : game_event)
            | 2 ->
              (End_game : game_event)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 6 -> (
                      if String.unsafe_get s pos = 'P' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'y' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 8 -> (
                      if String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'e' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 9 -> (
                      if String.unsafe_get s pos = 'E' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 't' then (
                        2
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 10 -> (
                      if String.unsafe_get s pos = 'I' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' then (
                        3
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 11 -> (
                      if String.unsafe_get s pos = 'R' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'v' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 't' then (
                        4
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_rule_descr
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Set_rule x : game_event)
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_player_descr
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Player x : game_event)
            | 2 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_player_idx
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (East_seat x : game_event)
            | 3 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Ag_oj_run.read_number
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Init_score x : game_event)
            | 4 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_round_event
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Round_event x : game_event)
            | _ -> (
                assert false
              )
        )
)
let game_event_of_string s =
  read_game_event (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__8 = (
  Ag_oj_run.write_list (
    write_round_event
  )
)
let string_of__8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__8 ob x;
  Bi_outbuf.contents ob
let read__8 = (
  Ag_oj_run.read_list (
    read_round_event
  )
)
let _8_of_string s =
  read__8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__7 = (
  Ag_oj_run.write_list (
    write_game_event
  )
)
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let read__7 = (
  Ag_oj_run.read_list (
    read_game_event
  )
)
let _7_of_string s =
  read__7 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_game : _ -> game -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"game_events\":";
    (
      write__7
    )
      ob x.game_events;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"current_round\":";
    (
      write__8
    )
      ob x.current_round;
    Bi_outbuf.add_char ob '}';
)
let string_of_game ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_game ob x;
  Bi_outbuf.contents ob
let read_game = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_game_events = ref (Obj.magic 0.0) in
    let field_current_round = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 11 -> (
                if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'v' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 13 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'u' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 'd' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_game_events := (
              (
                read__7
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_current_round := (
              (
                read__8
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 11 -> (
                  if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'v' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 13 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'u' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_game_events := (
                (
                  read__7
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_current_round := (
                (
                  read__8
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "game_events"; "current_round" |];
        (
          {
            game_events = !field_game_events;
            current_round = !field_current_round;
          }
         : game)
      )
)
let game_of_string s =
  read_game (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
