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
  | Init_score of int
  | Round_event of round_event
  | End_round
  | End_game


type game = Game_descr_t.game = {
  game_events: game_event list;
  current_round: round_event list
}

let tile_pos_tag = Bi_io.svint_tag
let write_untagged_tile_pos = (
  Bi_io.write_untagged_svint
)
let write_tile_pos ob x =
  Bi_io.write_tag ob Bi_io.svint_tag;
  write_untagged_tile_pos ob x
let string_of_tile_pos ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tile_pos ob x;
  Bi_outbuf.contents ob
let get_tile_pos_reader = (
  Ag_ob_run.get_int_reader
)
let read_tile_pos = (
  Ag_ob_run.read_int
)
let tile_pos_of_string ?pos s =
  read_tile_pos (Bi_inbuf.from_string ?pos s)
let _1_tag = Bi_io.string_tag
let write_untagged__1 = (
  fun ob x -> (
    let x = ( Tileset.string_of_tile ) x in (
      Bi_io.write_untagged_string
    ) ob x)
)
let write__1 ob x =
  Bi_io.write_tag ob Bi_io.string_tag;
  write_untagged__1 ob x
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let get__1_reader = (
  fun tag ib ->
    ( Tileset.tile_of_string ) ((
      Ag_ob_run.get_string_reader
    ) tag ib)
)
let read__1 = (
  fun ib ->
    ( Tileset.tile_of_string ) ((
      Ag_ob_run.read_string
    ) ib)
)
let _1_of_string ?pos s =
  read__1 (Bi_inbuf.from_string ?pos s)
let tile_tag = Bi_io.string_tag
let write_untagged_tile = (
  write_untagged__1
)
let write_tile ob x =
  Bi_io.write_tag ob Bi_io.string_tag;
  write_untagged_tile ob x
let string_of_tile ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tile ob x;
  Bi_outbuf.contents ob
let get_tile_reader = (
  get__1_reader
)
let read_tile = (
  read__1
)
let tile_of_string ?pos s =
  read_tile (Bi_inbuf.from_string ?pos s)
let _5_tag = Bi_io.array_tag
let write_untagged__5 = (
  Ag_ob_run.write_untagged_list
    Bi_io.string_tag
    (
      Bi_io.write_untagged_string
    )
)
let write__5 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__5 ob x
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let get__5_reader = (
  Ag_ob_run.get_list_reader (
    Ag_ob_run.get_string_reader
  )
)
let read__5 = (
  Ag_ob_run.read_list (
    Ag_ob_run.get_string_reader
  )
)
let _5_of_string ?pos s =
  read__5 (Bi_inbuf.from_string ?pos s)
let _6_tag = Bi_io.num_variant_tag
let write_untagged__6 = (
  Ag_ob_run.write_untagged_option (
    write__5
  )
)
let write__6 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__6 ob x
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let get__6_reader = (
  fun tag ->
    if tag <> 22 then Ag_ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                read__5
              )
                ib
            )
          | _ -> Ag_ob_run.read_error_at ib
)
let read__6 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Ag_ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            read__5
          )
            ib
        )
      | _ -> Ag_ob_run.read_error_at ib
)
let _6_of_string ?pos s =
  read__6 (Bi_inbuf.from_string ?pos s)
let rule_descr_tag = Bi_io.record_tag
let write_untagged_rule_descr : Bi_outbuf.t -> rule_descr -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\200' '\255' 'r' 'K';
    (
      Bi_io.write_string
    ) ob x.name;
    Bi_outbuf.add_char4 ob '\130' '\140' '+' 'G';
    (
      write__6
    ) ob x.flags;
)
let write_rule_descr ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_rule_descr ob x
let string_of_rule_descr ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_rule_descr ob x;
  Bi_outbuf.contents ob
let get_rule_descr_reader = (
  fun tag ->
    if tag <> 21 then Ag_ob_run.read_error () else
      fun ib ->
        let field_name = ref (Obj.magic 0.0) in
        let field_flags = ref (Obj.magic 0.0) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -922783157 ->
              field_name := (
                (
                  Ag_ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 42740551 ->
              field_flags := (
                (
                  read__6
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "name"; "flags" |];
        (
          {
            name = !field_name;
            flags = !field_flags;
          }
         : rule_descr)
)
let read_rule_descr = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Ag_ob_run.read_error_at ib;
    let field_name = ref (Obj.magic 0.0) in
    let field_flags = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -922783157 ->
          field_name := (
            (
              Ag_ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 42740551 ->
          field_flags := (
            (
              read__6
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "name"; "flags" |];
    (
      {
        name = !field_name;
        flags = !field_flags;
      }
     : rule_descr)
)
let rule_descr_of_string ?pos s =
  read_rule_descr (Bi_inbuf.from_string ?pos s)
let round_player_tag = Bi_io.svint_tag
let write_untagged_round_player = (
  Bi_io.write_untagged_svint
)
let write_round_player ob x =
  Bi_io.write_tag ob Bi_io.svint_tag;
  write_untagged_round_player ob x
let string_of_round_player ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_round_player ob x;
  Bi_outbuf.contents ob
let get_round_player_reader = (
  Ag_ob_run.get_int_reader
)
let read_round_player = (
  Ag_ob_run.read_int
)
let round_player_of_string ?pos s =
  read_round_player (Bi_inbuf.from_string ?pos s)
let _4_tag = Bi_io.array_tag
let write_untagged__4 = (
  Ag_ob_run.write_untagged_list
    tile_pos_tag
    (
      write_untagged_tile_pos
    )
)
let write__4 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__4 ob x
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let get__4_reader = (
  Ag_ob_run.get_list_reader (
    get_tile_pos_reader
  )
)
let read__4 = (
  Ag_ob_run.read_list (
    get_tile_pos_reader
  )
)
let _4_of_string ?pos s =
  read__4 (Bi_inbuf.from_string ?pos s)
let _2_tag = Bi_io.num_variant_tag
let write_untagged__2 = (
  Ag_ob_run.write_untagged_option (
    write_tile
  )
)
let write__2 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__2 ob x
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let get__2_reader = (
  fun tag ->
    if tag <> 22 then Ag_ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                read_tile
              )
                ib
            )
          | _ -> Ag_ob_run.read_error_at ib
)
let read__2 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Ag_ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            read_tile
          )
            ib
        )
      | _ -> Ag_ob_run.read_error_at ib
)
let _2_of_string ?pos s =
  read__2 (Bi_inbuf.from_string ?pos s)
let _3_tag = Bi_io.array_tag
let write_untagged__3 = (
  Ag_ob_run.write_untagged_array
    _2_tag
    (
      write_untagged__2
    )
)
let write__3 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__3 ob x
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let get__3_reader = (
  Ag_ob_run.get_array_reader (
    get__2_reader
  )
)
let read__3 = (
  Ag_ob_run.read_array (
    get__2_reader
  )
)
let _3_of_string ?pos s =
  read__3 (Bi_inbuf.from_string ?pos s)
let round_event_tag = Bi_io.variant_tag
let write_untagged_round_event : Bi_outbuf.t -> round_event -> unit = (
  fun ob x ->
    match x with
      | Init x ->
        Bi_outbuf.add_char4 ob '\176' '\148' 'g' '0';
        (
          write__3
        ) ob x
      | Wall_breaker_roll x ->
        Bi_outbuf.add_char4 ob '\148' '{' '\156' 'e';
        (
          Bi_io.write_svint
        ) ob x
      | Break_wall_roll x ->
        Bi_outbuf.add_char4 ob '\179' '\207' '\211' '2';
        (
          Bi_io.write_svint
        ) ob x
      | Deal -> Bi_outbuf.add_char4 ob '-' '?' 'z' '\204'
      | Draw x ->
        Bi_outbuf.add_char4 ob '\173' 'I' 'X' '$';
        (
          write_round_player
        ) ob x
      | Discard x ->
        Bi_outbuf.add_char4 ob '\255' '\251' 'A' '\254';
        (
          fun ob x ->
            Bi_io.write_tag ob Bi_io.tuple_tag;
            Bi_vint.write_uvint ob 2;
            (
              let x, _ = x in (
                write_round_player
              ) ob x
            );
            (
              let _, x = x in (
                write_tile_pos
              ) ob x
            );
        ) ob x
      | Mahjong x ->
        Bi_outbuf.add_char4 ob '\242' '\164' 'D' '\178';
        (
          write_round_player
        ) ob x
      | Concealed_kong x ->
        Bi_outbuf.add_char4 ob '\146' '\001' '\132' '\174';
        (
          fun ob x ->
            Bi_io.write_tag ob Bi_io.tuple_tag;
            Bi_vint.write_uvint ob 2;
            (
              let x, _ = x in (
                write_round_player
              ) ob x
            );
            (
              let _, x = x in (
                write__4
              ) ob x
            );
        ) ob x
      | Small_kong x ->
        Bi_outbuf.add_char4 ob '\136' '\196' 'l' '\181';
        (
          fun ob x ->
            Bi_io.write_tag ob Bi_io.tuple_tag;
            Bi_vint.write_uvint ob 2;
            (
              let x, _ = x in (
                write_round_player
              ) ob x
            );
            (
              let _, x = x in (
                write_tile_pos
              ) ob x
            );
        ) ob x
      | Chow x ->
        Bi_outbuf.add_char4 ob '\172' '\152' '\151' '-';
        (
          fun ob x ->
            Bi_io.write_tag ob Bi_io.tuple_tag;
            Bi_vint.write_uvint ob 2;
            (
              let x, _ = x in (
                write_round_player
              ) ob x
            );
            (
              let _, x = x in (
                write__4
              ) ob x
            );
        ) ob x
      | Pong x ->
        Bi_outbuf.add_char4 ob '\181' '5' '\172' '\024';
        (
          fun ob x ->
            Bi_io.write_tag ob Bi_io.tuple_tag;
            Bi_vint.write_uvint ob 2;
            (
              let x, _ = x in (
                write_round_player
              ) ob x
            );
            (
              let _, x = x in (
                write__4
              ) ob x
            );
        ) ob x
      | Kong x ->
        Bi_outbuf.add_char4 ob '\177' '\231' '\154' '\253';
        (
          fun ob x ->
            Bi_io.write_tag ob Bi_io.tuple_tag;
            Bi_vint.write_uvint ob 2;
            (
              let x, _ = x in (
                write_round_player
              ) ob x
            );
            (
              let _, x = x in (
                write__4
              ) ob x
            );
        ) ob x
      | No_action x ->
        Bi_outbuf.add_char4 ob '\151' '*' '\199' '\020';
        (
          write_round_player
        ) ob x
)
let write_round_event ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_round_event ob x
let string_of_round_event ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_round_event ob x;
  Bi_outbuf.contents ob
let get_round_event_reader = (
  fun tag ->
    if tag <> 23 then Ag_ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 815032112, true -> (Init (
                (
                  read__3
                ) ib
              ) : round_event)
            | 343645285, true -> (Wall_breaker_roll (
                (
                  Ag_ob_run.read_int
                ) ib
              ) : round_event)
            | 869258034, true -> (Break_wall_roll (
                (
                  Ag_ob_run.read_int
                ) ib
              ) : round_event)
            | 759134924, false -> (Deal : round_event)
            | 759781412, true -> (Draw (
                (
                  read_round_player
                ) ib
              ) : round_event)
            | -310786, true -> (Discard (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                    let len = Bi_vint.read_uvint ib in
                    if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                    let x0 =
                      (
                        read_round_player
                      ) ib
                    in
                    let x1 =
                      (
                        read_tile_pos
                      ) ib
                    in
                    for i = 2 to len - 1 do Bi_io.skip ib done;
                    (x0, x1)
                ) ib
              ) : round_event)
            | -224115534, true -> (Mahjong (
                (
                  read_round_player
                ) ib
              ) : round_event)
            | 302089390, true -> (Concealed_kong (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                    let len = Bi_vint.read_uvint ib in
                    if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                    let x0 =
                      (
                        read_round_player
                      ) ib
                    in
                    let x1 =
                      (
                        read__4
                      ) ib
                    in
                    for i = 2 to len - 1 do Bi_io.skip ib done;
                    (x0, x1)
                ) ib
              ) : round_event)
            | 147090613, true -> (Small_kong (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                    let len = Bi_vint.read_uvint ib in
                    if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                    let x0 =
                      (
                        read_round_player
                      ) ib
                    in
                    let x1 =
                      (
                        read_tile_pos
                      ) ib
                    in
                    for i = 2 to len - 1 do Bi_io.skip ib done;
                    (x0, x1)
                ) ib
              ) : round_event)
            | 748197677, true -> (Chow (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                    let len = Bi_vint.read_uvint ib in
                    if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                    let x0 =
                      (
                        read_round_player
                      ) ib
                    in
                    let x1 =
                      (
                        read__4
                      ) ib
                    in
                    for i = 2 to len - 1 do Bi_io.skip ib done;
                    (x0, x1)
                ) ib
              ) : round_event)
            | 892709912, true -> (Pong (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                    let len = Bi_vint.read_uvint ib in
                    if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                    let x0 =
                      (
                        read_round_player
                      ) ib
                    in
                    let x1 =
                      (
                        read__4
                      ) ib
                    in
                    for i = 2 to len - 1 do Bi_io.skip ib done;
                    (x0, x1)
                ) ib
              ) : round_event)
            | 837262077, true -> (Kong (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                    let len = Bi_vint.read_uvint ib in
                    if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                    let x0 =
                      (
                        read_round_player
                      ) ib
                    in
                    let x1 =
                      (
                        read__4
                      ) ib
                    in
                    for i = 2 to len - 1 do Bi_io.skip ib done;
                    (x0, x1)
                ) ib
              ) : round_event)
            | 388679444, true -> (No_action (
                (
                  read_round_player
                ) ib
              ) : round_event)
            | _ -> Ag_ob_run.unsupported_variant h has_arg
        )
)
let read_round_event = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Ag_ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 815032112, true -> (Init (
            (
              read__3
            ) ib
          ) : round_event)
        | 343645285, true -> (Wall_breaker_roll (
            (
              Ag_ob_run.read_int
            ) ib
          ) : round_event)
        | 869258034, true -> (Break_wall_roll (
            (
              Ag_ob_run.read_int
            ) ib
          ) : round_event)
        | 759134924, false -> (Deal : round_event)
        | 759781412, true -> (Draw (
            (
              read_round_player
            ) ib
          ) : round_event)
        | -310786, true -> (Discard (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                let len = Bi_vint.read_uvint ib in
                if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                let x0 =
                  (
                    read_round_player
                  ) ib
                in
                let x1 =
                  (
                    read_tile_pos
                  ) ib
                in
                for i = 2 to len - 1 do Bi_io.skip ib done;
                (x0, x1)
            ) ib
          ) : round_event)
        | -224115534, true -> (Mahjong (
            (
              read_round_player
            ) ib
          ) : round_event)
        | 302089390, true -> (Concealed_kong (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                let len = Bi_vint.read_uvint ib in
                if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                let x0 =
                  (
                    read_round_player
                  ) ib
                in
                let x1 =
                  (
                    read__4
                  ) ib
                in
                for i = 2 to len - 1 do Bi_io.skip ib done;
                (x0, x1)
            ) ib
          ) : round_event)
        | 147090613, true -> (Small_kong (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                let len = Bi_vint.read_uvint ib in
                if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                let x0 =
                  (
                    read_round_player
                  ) ib
                in
                let x1 =
                  (
                    read_tile_pos
                  ) ib
                in
                for i = 2 to len - 1 do Bi_io.skip ib done;
                (x0, x1)
            ) ib
          ) : round_event)
        | 748197677, true -> (Chow (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                let len = Bi_vint.read_uvint ib in
                if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                let x0 =
                  (
                    read_round_player
                  ) ib
                in
                let x1 =
                  (
                    read__4
                  ) ib
                in
                for i = 2 to len - 1 do Bi_io.skip ib done;
                (x0, x1)
            ) ib
          ) : round_event)
        | 892709912, true -> (Pong (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                let len = Bi_vint.read_uvint ib in
                if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                let x0 =
                  (
                    read_round_player
                  ) ib
                in
                let x1 =
                  (
                    read__4
                  ) ib
                in
                for i = 2 to len - 1 do Bi_io.skip ib done;
                (x0, x1)
            ) ib
          ) : round_event)
        | 837262077, true -> (Kong (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
                let len = Bi_vint.read_uvint ib in
                if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
                let x0 =
                  (
                    read_round_player
                  ) ib
                in
                let x1 =
                  (
                    read__4
                  ) ib
                in
                for i = 2 to len - 1 do Bi_io.skip ib done;
                (x0, x1)
            ) ib
          ) : round_event)
        | 388679444, true -> (No_action (
            (
              read_round_player
            ) ib
          ) : round_event)
        | _ -> Ag_ob_run.unsupported_variant h has_arg
    )
)
let round_event_of_string ?pos s =
  read_round_event (Bi_inbuf.from_string ?pos s)
let ai_conf_tag = Bi_io.record_tag
let write_untagged_ai_conf : Bi_outbuf.t -> ai_conf -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\200' '\255' 'r' 'K';
    (
      Bi_io.write_string
    ) ob x.name;
    Bi_outbuf.add_char4 ob '\132' '\148' '\177' '\235';
    (
      Bi_io.write_svint
    ) ob x.force;
)
let write_ai_conf ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_ai_conf ob x
let string_of_ai_conf ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ai_conf ob x;
  Bi_outbuf.contents ob
let get_ai_conf_reader = (
  fun tag ->
    if tag <> 21 then Ag_ob_run.read_error () else
      fun ib ->
        let field_name = ref (Obj.magic 0.0) in
        let field_force = ref (Obj.magic 0.0) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -922783157 ->
              field_name := (
                (
                  Ag_ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 76853739 ->
              field_force := (
                (
                  Ag_ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "name"; "force" |];
        (
          {
            name = !field_name;
            force = !field_force;
          }
         : ai_conf)
)
let read_ai_conf = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Ag_ob_run.read_error_at ib;
    let field_name = ref (Obj.magic 0.0) in
    let field_force = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -922783157 ->
          field_name := (
            (
              Ag_ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 76853739 ->
          field_force := (
            (
              Ag_ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "name"; "force" |];
    (
      {
        name = !field_name;
        force = !field_force;
      }
     : ai_conf)
)
let ai_conf_of_string ?pos s =
  read_ai_conf (Bi_inbuf.from_string ?pos s)
let player_kind_tag = Bi_io.variant_tag
let write_untagged_player_kind : Bi_outbuf.t -> player_kind -> unit = (
  fun ob x ->
    match x with
      | Human -> Bi_outbuf.add_char4 ob 'B' '\130' '\197' '\141'
      | AI x ->
        Bi_outbuf.add_char4 ob '\128' '\000' '8' '\232';
        (
          write_ai_conf
        ) ob x
)
let write_player_kind ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_player_kind ob x
let string_of_player_kind ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player_kind ob x;
  Bi_outbuf.contents ob
let get_player_kind_reader = (
  fun tag ->
    if tag <> 23 then Ag_ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | -1031617139, false -> (Human : player_kind)
            | 14568, true -> (AI (
                (
                  read_ai_conf
                ) ib
              ) : player_kind)
            | _ -> Ag_ob_run.unsupported_variant h has_arg
        )
)
let read_player_kind = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Ag_ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | -1031617139, false -> (Human : player_kind)
        | 14568, true -> (AI (
            (
              read_ai_conf
            ) ib
          ) : player_kind)
        | _ -> Ag_ob_run.unsupported_variant h has_arg
    )
)
let player_kind_of_string ?pos s =
  read_player_kind (Bi_inbuf.from_string ?pos s)
let player_idx_tag = Bi_io.svint_tag
let write_untagged_player_idx = (
  Bi_io.write_untagged_svint
)
let write_player_idx ob x =
  Bi_io.write_tag ob Bi_io.svint_tag;
  write_untagged_player_idx ob x
let string_of_player_idx ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player_idx ob x;
  Bi_outbuf.contents ob
let get_player_idx_reader = (
  Ag_ob_run.get_int_reader
)
let read_player_idx = (
  Ag_ob_run.read_int
)
let player_idx_of_string ?pos s =
  read_player_idx (Bi_inbuf.from_string ?pos s)
let player_descr_tag = Bi_io.record_tag
let write_untagged_player_descr : Bi_outbuf.t -> player_descr -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\200' '\255' 'r' 'K';
    (
      Bi_io.write_string
    ) ob x.name;
    Bi_outbuf.add_char4 ob '\199' '\t' '\225' 'T';
    (
      write_player_kind
    ) ob x.kind;
)
let write_player_descr ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_player_descr ob x
let string_of_player_descr ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player_descr ob x;
  Bi_outbuf.contents ob
let get_player_descr_reader = (
  fun tag ->
    if tag <> 21 then Ag_ob_run.read_error () else
      fun ib ->
        let field_name = ref (Obj.magic 0.0) in
        let field_kind = ref (Obj.magic 0.0) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -922783157 ->
              field_name := (
                (
                  Ag_ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | -955653804 ->
              field_kind := (
                (
                  read_player_kind
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "name"; "kind" |];
        (
          {
            name = !field_name;
            kind = !field_kind;
          }
         : player_descr)
)
let read_player_descr = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Ag_ob_run.read_error_at ib;
    let field_name = ref (Obj.magic 0.0) in
    let field_kind = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -922783157 ->
          field_name := (
            (
              Ag_ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | -955653804 ->
          field_kind := (
            (
              read_player_kind
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "name"; "kind" |];
    (
      {
        name = !field_name;
        kind = !field_kind;
      }
     : player_descr)
)
let player_descr_of_string ?pos s =
  read_player_descr (Bi_inbuf.from_string ?pos s)
let game_event_tag = Bi_io.variant_tag
let write_untagged_game_event : Bi_outbuf.t -> game_event -> unit = (
  fun ob x ->
    match x with
      | Set_rule x ->
        Bi_outbuf.add_char4 ob '\211' '}' 'J' '\025';
        (
          write_rule_descr
        ) ob x
      | Player x ->
        Bi_outbuf.add_char4 ob '\236' 'M' '_' '\129';
        (
          write_player_descr
        ) ob x
      | East_seat x ->
        Bi_outbuf.add_char4 ob '\150' '\026' '\\' '\167';
        (
          write_player_idx
        ) ob x
      | Init_score x ->
        Bi_outbuf.add_char4 ob '\187' '"' '\165' '\195';
        (
          Bi_io.write_svint
        ) ob x
      | Round_event x ->
        Bi_outbuf.add_char4 ob '\211' '\152' '\206' '\t';
        (
          write_round_event
        ) ob x
      | End_round -> Bi_outbuf.add_char4 ob '\022' 'm' '\249' '\138'
      | End_game -> Bi_outbuf.add_char4 ob ';' '\241' '\238' '6'
)
let write_game_event ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_game_event ob x
let string_of_game_event ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_game_event ob x;
  Bi_outbuf.contents ob
let get_game_event_reader = (
  fun tag ->
    if tag <> 23 then Ag_ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | -746763751, true -> (Set_rule (
                (
                  read_rule_descr
                ) ib
              ) : game_event)
            | -330473599, true -> (Player (
                (
                  read_player_descr
                ) ib
              ) : game_event)
            | 370826407, true -> (East_seat (
                (
                  read_player_idx
                ) ib
              ) : game_event)
            | 992126403, true -> (Init_score (
                (
                  Ag_ob_run.read_int
                ) ib
              ) : game_event)
            | -744960503, true -> (Round_event (
                (
                  read_round_event
                ) ib
              ) : game_event)
            | 376306058, false -> (End_round : game_event)
            | 1005710902, false -> (End_game : game_event)
            | _ -> Ag_ob_run.unsupported_variant h has_arg
        )
)
let read_game_event = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Ag_ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | -746763751, true -> (Set_rule (
            (
              read_rule_descr
            ) ib
          ) : game_event)
        | -330473599, true -> (Player (
            (
              read_player_descr
            ) ib
          ) : game_event)
        | 370826407, true -> (East_seat (
            (
              read_player_idx
            ) ib
          ) : game_event)
        | 992126403, true -> (Init_score (
            (
              Ag_ob_run.read_int
            ) ib
          ) : game_event)
        | -744960503, true -> (Round_event (
            (
              read_round_event
            ) ib
          ) : game_event)
        | 376306058, false -> (End_round : game_event)
        | 1005710902, false -> (End_game : game_event)
        | _ -> Ag_ob_run.unsupported_variant h has_arg
    )
)
let game_event_of_string ?pos s =
  read_game_event (Bi_inbuf.from_string ?pos s)
let _8_tag = Bi_io.array_tag
let write_untagged__8 = (
  Ag_ob_run.write_untagged_list
    round_event_tag
    (
      write_untagged_round_event
    )
)
let write__8 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__8 ob x
let string_of__8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__8 ob x;
  Bi_outbuf.contents ob
let get__8_reader = (
  Ag_ob_run.get_list_reader (
    get_round_event_reader
  )
)
let read__8 = (
  Ag_ob_run.read_list (
    get_round_event_reader
  )
)
let _8_of_string ?pos s =
  read__8 (Bi_inbuf.from_string ?pos s)
let _7_tag = Bi_io.array_tag
let write_untagged__7 = (
  Ag_ob_run.write_untagged_list
    game_event_tag
    (
      write_untagged_game_event
    )
)
let write__7 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__7 ob x
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let get__7_reader = (
  Ag_ob_run.get_list_reader (
    get_game_event_reader
  )
)
let read__7 = (
  Ag_ob_run.read_list (
    get_game_event_reader
  )
)
let _7_of_string ?pos s =
  read__7 (Bi_inbuf.from_string ?pos s)
let game_tag = Bi_io.record_tag
let write_untagged_game : Bi_outbuf.t -> game -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\156' 'F' '\175' '\006';
    (
      write__7
    ) ob x.game_events;
    Bi_outbuf.add_char4 ob '\152' '\021' 'L' '\136';
    (
      write__8
    ) ob x.current_round;
)
let write_game ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_game ob x
let string_of_game ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_game ob x;
  Bi_outbuf.contents ob
let get_game_reader = (
  fun tag ->
    if tag <> 21 then Ag_ob_run.read_error () else
      fun ib ->
        let field_game_events = ref (Obj.magic 0.0) in
        let field_current_round = ref (Obj.magic 0.0) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 474394374 ->
              field_game_events := (
                (
                  read__7
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 404049032 ->
              field_current_round := (
                (
                  read__8
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "game_events"; "current_round" |];
        (
          {
            game_events = !field_game_events;
            current_round = !field_current_round;
          }
         : game)
)
let read_game = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Ag_ob_run.read_error_at ib;
    let field_game_events = ref (Obj.magic 0.0) in
    let field_current_round = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 474394374 ->
          field_game_events := (
            (
              read__7
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 404049032 ->
          field_current_round := (
            (
              read__8
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Ag_ob_run.missing_fields [| !bits0 |] [| "game_events"; "current_round" |];
    (
      {
        game_events = !field_game_events;
        current_round = !field_current_round;
      }
     : game)
)
let game_of_string ?pos s =
  read_game (Bi_inbuf.from_string ?pos s)
