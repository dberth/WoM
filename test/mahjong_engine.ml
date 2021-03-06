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

   "Seven pairs all differents", 4,
   [d1; d1; c1; c1; b1; b1; rd; rd; gd; gd; ww; ww; nw; nw],
   [[[Char 1; Char 1]; [Bam 1; Bam 1]; [Dot 1; Dot 1]; [Red_dragon; Red_dragon]; [Green_dragon; Green_dragon]; [North_wind; North_wind]; [West_wind; West_wind]]];

   "Seven pairs with double pairs", 4,
   [d1; d1; c1; c1; b1; b1; rd; rd; gd; gd; ww; ww; ww; ww],
   [[[Char 1; Char 1]; [Bam 1; Bam 1]; [Dot 1; Dot 1]; [Red_dragon; Red_dragon]; [Green_dragon; Green_dragon]; [West_wind; West_wind]; [West_wind; West_wind]];
   [[Char 1; Char 1]; [Bam 1; Bam 1]; [Dot 1; Dot 1]; [Red_dragon; Red_dragon]; [Green_dragon; Green_dragon]; [West_wind; West_wind]; [West_wind; West_wind]]]
  ] |>
    List.map
      (fun (title, nb_set, tiles, descr) ->
        title >:: fun _ctx ->
          assert_equal ~printer: pp_mahjong_list descr (mahjong ~seven_pairs: true ~irregular_hands nb_set (tileset_of_tiles tiles) |> List.map tile_descr_of_mahjong)
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

let zj_irreg_hand_test hand expected =
  let mahjong = Irregular (Tileset.tileset_of_tiles hand) in
  let explanations, score = Zung_jung.mahjong_pts (fun _ -> true) [] ww (Some c1) (Tileset.tileset_of_tiles [c1]) mahjong [] in
  if false (*DEBUG*) then begin
    print_endline "======";
    List.iter
      (fun (s, x) ->
         print_endline (Printf.sprintf "%s: %.0f" s x)
      )
      explanations
  end;
  assert_equal ~printer: string_of_float expected score

let zj_reg_hand_test ?(seat_wind = ww) ?(extraordinary_events = []) hand declared expected =
  let mahjong = Regular (List.map basic_tileset_of_tiles hand) in
  let declared = List.map (fun (x, y) -> tileset_of_tiles x, [], y) declared in
  let explanations, score = Zung_jung.mahjong_pts (fun _ -> true) extraordinary_events seat_wind (Some c1) (Tileset.tileset_of_tiles [c1]) mahjong declared in
  if false (*DEBUG*) then begin
    print_endline "======";
    List.iter
      (fun (s, x) ->
         print_endline (Printf.sprintf "%s: %.0f" s x)
      )
      explanations
  end;
  assert_equal ~printer: string_of_float expected score

let chicken_hand _ctx =
  zj_reg_hand_test
    [[d1; d2; d3]; [c1; c1; c1]; [gd; gd]]
    [[d5; d5; d5], false; [b2; b2; b2], false]
    1.

let concealed_hand _ctx =
  zj_reg_hand_test
    [[d1; d2; d3]; [b2; b3; b4]; [c2; c3; c4]; [d9; d9; d9]; [b8; b8]]
    []
    5.

let concealed_hand_with_kong _ctx =
  zj_reg_hand_test
    [[d1; d2; d3]; [b2; b3; b4]; [c2; c3; c4]; [b8; b8]]
    [[d9; d9; d9; d9], true]
    10.

let all_chows _ctx =
  zj_reg_hand_test
    [[d1; d2; d3]; [b2; b3; b4]; [c2; c3; c4]; [b8; b8]]
    [[c7; c8; c9], false]
    5.

let no_terminals _ctx =
  zj_reg_hand_test
    [[d2; d3; d4]; [c7; c7; c7]; [gd; gd]]
    [[d5; d5; d5], false; [b2; b2; b2], false]
    5.

let all_chows_no_terminals _ctx =
  zj_reg_hand_test
    [[d3; d4; d5]; [b2; b3; b4]; [c2; c3; c4]; [b8; b8]]
    [[c6; c7; c8], false]
    10.

let all_rules_1 _ctx =
  zj_reg_hand_test
    [[d3; d4; d5]; [b2; b3; b4]; [c2; c3; c4]; [b8; b8]; [c6; c7; c8]]
    []
    15.

let rules_1 =
  "Rules 1" >:::
  [
    "Conceald hand" >:: concealed_hand;
    "Concealed hand with kong" >:: concealed_hand_with_kong;
    "All chows" >:: all_chows;
    "No terminals" >:: no_terminals;
    "All chows + No terminals" >:: all_chows_no_terminals;
    "All chows + Concealed hand + No terminals" >:: all_rules_1
  ]

let mixed_one_suit _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [c2; c3; c4]; [gd; gd]; [c9; c9; c9]]
    [[c7; c7; c7], false]
    40.

let pure_one_suit _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [c2; c3; c4]; [c5; c5]; [c9; c9; c9]]
    [[c7; c7; c7], false]
    80.

let nine_gates _ctx =
    assert_equal ~printer: string_of_float 480. (snd (Zung_jung.mahjong_pts (fun _ -> true) [] ww (Some c1) (Tileset.tileset_of_tiles [c1; c1; c1; c1; c2; c3; c4; c5; c6; c7; c8; c9; c9; c9]) (Regular (List.map basic_tileset_of_tiles [[c1; c1; c1]; [c1; c2; c3]; [c4; c5; c6]; [c7; c8; c9]; [c9; c9]])) []))

let rules_2 =
  "Rules 2" >:::
  [
    "Mixed One-Suit" >:: mixed_one_suit;
    "Pure One-Suit" >:: pure_one_suit;
    "Nine gates" >:: nine_gates
  ]

let value_honor _ctx =
  zj_reg_hand_test
    ~seat_wind: ew
    [[d1; d1]; [c2; c3; c4] ; [ww; ww; ww]]
    [[gd; gd; gd], false; [ew; ew; ew], false]
    20.

let small_three_dragons _ctx =
  zj_reg_hand_test
    [[rd; rd]; [c1; c2; c3]; [gd; gd; gd]; [d2; d3; d4]]
    [[wd; wd; wd], false]
    60.

let big_three_dragons _ctx =
  zj_reg_hand_test
    [[rd; rd; rd]; [b1; b1]; [c2; c3; c4]]
    [[wd; wd; wd], false; [gd; gd; gd], false]
    160.

let small_three_winds _ctx =
  zj_reg_hand_test
    ~seat_wind: sw
    [[ww; ww; ww]; [nw; nw]; [c1; c2; c3]]
    [[d2; d3; d4], false; [ew; ew; ew], false]
    30.

let big_three_winds _ctx =
  zj_reg_hand_test
    ~seat_wind: ew
    [[ww; ww; ww]; [b1; b1]; [c2; c3; c4]]
    [[sw; sw; sw], false; [nw; nw; nw], false]
    120.

let small_four_winds _ctx =
  zj_reg_hand_test
    [[ww; ww; ww]; [ew; ew; ew]; [nw; nw; nw]; [sw; sw]]
    [[c1; c2; c3], false]
    320.

let big_four_winds _ctx =
  zj_reg_hand_test
    [[ww; ww; ww]; [ew; ew; ew]; [nw; nw; nw]; [sw; sw; sw; sw]; [b2; b2]]
    []
    400.

let all_honors _ctx =
  zj_reg_hand_test
    [[rd; rd; rd]; [gd; gd; gd]; [ew; ew; ew]; [sw; sw; sw]; [ww; ww]]
    []
    320.

let rules_3 =
  "Rules 3" >:::
  [
    "Value Honor" >:: value_honor;
    "Small Three Dragons" >:: small_three_dragons;
    "Big Three Dragons" >:: big_three_dragons;
    "Small Three Winds" >:: small_three_winds;
    "Big Three Winds" >:: big_three_winds;
    "Small Four Winds" >:: small_four_winds;
    "Big Four Winds" >:: big_four_winds;
    "All honors" >:: all_honors
  ]

let all_pong _ctx =
  zj_reg_hand_test
    [[nw; nw]]
    [[b5; b5; b5], false; [d1; d1; d1], false; [c2; c2; c2], false; [d3; d3; d3], false]
    30.

let two_concealed_pong _ctx =
  zj_reg_hand_test
    [[d1; d1; d1]; [c3; c3; c3]; [b5; b5]; [b1; b2; b3]]
    [[d3; d4; d5], false]
    5.

let three_concealed_pong _ctx =
  zj_reg_hand_test
    [[d1; d1; d1]; [c3; c3; c3]; [b5; b5]; [b2; b2; b2]]
    [[d3; d4; d5], false]
    30.

let four_concealed_pong _ctx =
  zj_reg_hand_test
    [[d1; d1; d1]; [c3; c3; c3]; [b5; b5]; [b2; b2; b2]; [nw; nw; nw]]
    []
    160.

let one_kong _ctx =
  zj_reg_hand_test
    [[d1; d1; d1]; [b5; b5]; [c1; c2; c3]]
    [[d6; d6; d6; d6], false; [d6; d7; d8], false]
    5.

let two_kong _ctx =
  zj_reg_hand_test
    [[d1; d1; d1]; [b5; b5]; [c1; c2; c3]]
    [[d6; d6; d6; d6], false; [c9; c9; c9; c9], false]
    20.

let three_kong _ctx =
  zj_reg_hand_test
    [[b5; b5]; [c1; c2; c3]]
    [[d6; d6; d6; d6], false; [c9; c9; c9; c9], false; [d1; d1; d1; d1], false]
    120.

let four_kong _ctx =
  zj_reg_hand_test
    [[b5; b5]]
    [[d6; d6; d6; d6], false; [c9; c9; c9; c9], false; [d1; d1; d1; d1], false; [c1; c1; c1; c1], true]
    480.


let rules_4 =
  "Rules 4" >:::
  [
    "All Pong and Kong" >:: all_pong;
    "Two concealed Pong" >:: two_concealed_pong;
    "Three concealed Pong" >:: three_concealed_pong;
    "Four concealed Pong" >:: four_concealed_pong;
    "One Kong" >:: one_kong;
    "Two Kong" >:: two_kong;
    "Three Kong" >:: three_kong;
    "Four Kong" >:: four_kong;
  ]

let two_identical_chow _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [c1; c2; c3]; [b3; b3; b3]; [b5; b5]]
    [[d2; d3; d4], false]
    10.

let two_identical_chow_twice _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [c1; c2; c3]; [d2; d3; d4]; [b5; b5]]
    [[d2; d3; d4], false]
    65.

let three_identical_chow _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [c1; c2; c3]; [c1; c2; c3]; [b5; b5]]
    [[d3; d3; d3], false]
    120.

let four_identical_chow _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [c1; c2; c3]; [c1; c2; c3]; [b5; b5]]
    [[c1; c2; c3], false]
    480.

let rules_5 =
  "Rules 5" >:::
  [
    "Two identical chow" >:: two_identical_chow;
    "Two identical chow twice" >:: two_identical_chow_twice;
    "Three identical chow" >:: three_identical_chow;
    "Four identical chow" >:: four_identical_chow;
  ]

let three_similar_chow _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [b1; b2; b3]; [d1; d2; d3]; [ww; ww]]
    [[b5; b5; b5], false]
    35.

let small_three_similar_pong_or_kong _ctx =
  zj_reg_hand_test
    [[c5; c5; c5]; [c1; c2; c3]; [b5; b5]]
    [[d5; d5; d5], false; [b6; b7; b8], false]
    30.

let three_similar_pong_or_kong _ctx =
  zj_reg_hand_test
    [[c5; c5; c5]; [c1; c2; c3]; [ww; ww]]
    [[d5; d5; d5], false; [b5; b5; b5], false]
    120.

let rules_6 =
  "Rules 6" >:::
  [
    "Three similar chow" >:: three_similar_chow;
    "Small Three Similar Pong or Kong" >:: small_three_similar_pong_or_kong;
    "Three Similar Pong or Kong" >:: three_similar_pong_or_kong;
  ]

let nine_tile_straight _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [c4; c5; c6]; [b2; b2]; [b5; b5; b5]]
    [[c7; c8; c9], false]
    40.

let three_consecutive_pong_or_kong _ctx =
  zj_reg_hand_test
    [[c1; c1; c1]; [d2; d3; d4]; [ww; ww]]
    [[c2; c2; c2], false; [c3; c3; c3], false]
    100.

let four_consecutive_pong_or_kong _ctx =
  zj_reg_hand_test
    [[c1; c1; c1]; [b1; b1]]
    [[c2; c2; c2], false; [c3; c3; c3], false; [c4; c4; c4], false]
    230.


let rules_7 =
  "Rules 7" >:::
  [
    "Nine-Tile Straight" >:: nine_tile_straight;
    "Three consecutive pong or kong" >:: three_consecutive_pong_or_kong;
    "Four consecutive pong or kong" >:: four_consecutive_pong_or_kong;
  ]

let mixed_lesser_terminals _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [d7; d8; d9]; [ww; ww]]
    [[b1; b1; b1], false; [c9; c9; c9], false]
    40.

let pure_lesser_terminals _ctx =
  zj_reg_hand_test
    [[c1; c2; c3]; [d7; d8; d9]; [c1; c1]]
    [[b1; b1; b1], false; [c9; c9; c9], false]
    50.

let mixed_greater_terminals _ctx =
  zj_reg_hand_test
    [[b1; b1; b1]; [ww; ww]]
    [[c1; c1; c1], false; [b9; b9; b9], false; [nw; nw; nw], false]
    130.

let pure_greater_terminals _ctx =
  zj_reg_hand_test
    [[b1; b1; b1]; [d1; d1]]
    [[c1; c1; c1], false; [b9; b9; b9], false;[c9; c9; c9], false]
    400.

let rules_8 =
  "Rules 8" >:::
  [
    "Mixed Lesser terminals" >:: mixed_lesser_terminals;
    "Pure Lesser terminals" >:: pure_lesser_terminals;
    "Mixed Greater terminals" >:: mixed_greater_terminals;
    "PUre Greater terminals" >:: pure_greater_terminals
  ]

let bonus event pts _ctx =
  zj_reg_hand_test
    ~extraordinary_events: [event]
    [[d1; d2; d3]; [c1; c1; c1]; [gd; gd]]
    [[d5; d5; d5], false; [b2; b2; b2], false]
    pts
    

let rules_9 =
  let open Engine in
  "Rules 9" >:::
  [
    "Final Draw" >:: bonus Final_draw 10.;
    "Final Discard" >:: bonus Final_discard 10.;
    "Win on Kong" >:: bonus Win_on_kong 10.;
    "Kong robbing" >:: bonus Kong_robbing 10.;
    "Blessing of Heaven" >:: bonus Blessing_of_heaven 155.;
    "Blessing of earth" >:: bonus Blessing_of_earth 155.;
  ]

let seven_pairs _ctx =
  zj_reg_hand_test
    [[d1; d1]; [d5; d5]; [ww; ww]; [c2; c2]; [b7; b7]; [b3; b3]; [gd; gd]]
    []
    30.

let thirteen_terminals _ctx =
  zj_irreg_hand_test
    [b1; b9; c1; c9; d1; d9; wd; rd; gd; ew; sw; ww; nw; nw]
    160.

let rules_10 =
  "Rules 10" >:::
  [
    "Seven Pairs" >:: seven_pairs;
    "Thirteen terminals" >:: thirteen_terminals
  ]

let misc_1 _ctx =
  zj_reg_hand_test
    [[d3; d4; d5]; [d7; d7]]
    [[c6; c7; c8], false; [wd; wd; wd], false; [b5; b5; b5], false]
    15.

let misc_2 _ctx =
  zj_reg_hand_test
    [[c3; c4; c5]; [c8; c8]; [c9; c9; c9]]
    [[gd; gd; gd], false; [c2; c2; c2], false]
    50.

let misc =
  "Misc" >:::
  [
    "Misc 1" >:: misc_1;
    "Misc 2" >:: misc_2
  ]


let zung_jung_suite =
  "Zung Jung rules test suite" >:::
  [
    "Chicken hand" >:: chicken_hand;
    rules_1;
    rules_2;
    rules_3;
    rules_4;
    rules_5;
    rules_6;
    rules_7;
    rules_8;
    rules_9;
    rules_10;
    misc
  ]

let engine_suite =
  "Mahjong engine test suite" >:::
    [
      tileset_test_suite;
      fsm_test_suite;
      zung_jung_suite;
    ]


let () = run_test_tt_main engine_suite
