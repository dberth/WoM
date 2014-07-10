(* Copyright (C) 2014 Denis Berthod *)

open OUnit2
open Tileset

let pp_tile_descr_list x =
  List.map string_of_tile_descr x |>
    String.concat "; " |> Printf.sprintf "[%s]"

let pp_mahjong x =
  List.map pp_tile_descr_list x |> String.concat " "

let pp_mahjong_list x =
  List.map pp_mahjong x |> String.concat "\n"

let tile_description =
  [
    "b1", Bam 1, b1;
    "rd", Red_dragon, rd;
    "ww", West_wind, ww;
  ]|>
    List.map
      (fun (title, descr, tile) ->
        title >:: fun _ctx ->
          assert_equal ~printer: string_of_tile_descr descr (tile_descr_of_tile tile)
      )

let mahjong =
  let irregular_without_lonely = [d1;d1;c2;c2;d3;d3;d4;d4;c5;c5;b6;b6;ww;ww] in
  let irregular_with_lonely = [d1; c1; b1; d9; c9; b9; wd; gd; rd; ww; ew; sw; nw; nw] in
  let irregular_hands =
    no_irregular_hand |>
      add_irregular_hand irregular_without_lonely |>
      add_irregular_hand irregular_with_lonely
  in
  ["Regular mahjong 4", 4,
   [b1; b2; b3; c1; c2; c3; rd; rd; rd; ww; ww; ww; d1; d1],
   [[[Dot 1; Dot 1]; [West_wind; West_wind; West_wind]; [Red_dragon; Red_dragon; Red_dragon]; [Bam 1; Bam 2; Bam 3]; [Char 1; Char 2; Char 3]]];

   "Regular mahjong 3", 3,
   [rd; rd; rd; c3; c4; c5; d9; d9; d9; ww; ww],
   [[[West_wind; West_wind]; [Red_dragon; Red_dragon; Red_dragon]; [Dot 9; Dot 9; Dot 9]; [Char 3; Char 4; Char 5]]];

   "Regular mahjong 2", 2,
   [gd; gd; gd; d4; d5; d6; wd; wd],
   [[[White_dragon; White_dragon]; [Green_dragon; Green_dragon; Green_dragon]; [Dot 4; Dot 5; Dot 6]]];

   "Regular mahjong 1", 1,
   [d1; d1; gd; gd; gd],
   [[[Dot 1; Dot 1];[Green_dragon; Green_dragon; Green_dragon]]];

   "Regular mahjong 0", 0,
   [c9; c9],
   [[[Char 9; Char 9]]];

   "Not a mahjong", 4,
   [b1; b2; b3; c1; c2; c3; rd; rd; rd; ww; ww; ww; d1; d2],
   [];

   "Irregular without lonely", 4,
   irregular_without_lonely,
   [[[Char 2; Char 2; Char 5; Char 5; Bam 6; Bam 6; Dot 1; Dot 1; Dot 3; Dot 3; Dot 4; Dot 4; West_wind; West_wind]]];

   "Irregular with lonely", 4,
   irregular_with_lonely,
   [[[Char 1; Char 9; Bam 1; Bam 9; Dot 1; Dot 9; Red_dragon; Green_dragon; White_dragon; East_wind; South_wind; North_wind; North_wind; West_wind]]];

  ] |>
    List.map
      (fun (title, nb_set, tiles, descr) ->
        title >:: fun _ctx ->
          assert_equal ~printer: pp_mahjong_list descr (mahjong ~ irregular_hands nb_set (tileset_of_tiles tiles) |> List.map tile_descr_of_mahjong)
      )



let tileset_test_suite =
  "Tileset test suite" >:::
    [
      ("tile_descr_of_tile" >::: tile_description);
      ("mahjong" >::: mahjong);
    ]

module Red_light = struct
  open Fsm

  let rec red = lazy (new_state (function `TIMEOUT -> green))

  and green = lazy (new_state (function `TIMEOUT -> orange))

  and orange = lazy(new_state (function `TIMEOUT -> red))

  let state_handlers =
    let entry light_color =
      fun _event _previous_light_color -> light_color
    in
    empty_action_handler |>
      on_entry red (entry `RED) |>
      on_entry green (entry `GREEN) |>
      on_entry orange (entry `ORANGE)

  let string_of_light = function
    | `RED -> "RED"
    | `GREEN -> "GREEN"
    | `ORANGE -> "ORANGE"

  let test _ctx =
    assert_equal
      ~printer: string_of_light
      (`GREEN)
      (fst (run state_handlers `RED red [`TIMEOUT; `TIMEOUT; `TIMEOUT; `TIMEOUT]))

end

let fsm_test_suite =
  "Finite State Machine" >:::
    [
      "Red light" >:: Red_light.test;
    ]

let engine_suite =
  "Mahjong engine test suite" >:::
    [
      tileset_test_suite;
      fsm_test_suite
    ]

let () = run_test_tt_main engine_suite
