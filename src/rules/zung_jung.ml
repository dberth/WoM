(*Copyright (C) 2014 Denis Berthod*)

(*
1.0 Trivial patterns

   1.1   All Sequences                 5
   1.2   Concealed hand                5
   1.3   No terminals                  5

2.0 One-Suit patterns

   2.1.1 Mixes One-Suit                40
   2.1.2 Pure One-Suit                 80
   2.2   Nine Gates                    480

3.0 Honor Tiles

   3.1   Value Honor                   10 per set
   3.2.1 Small Three Dragons           40
   3.2.2 Big Three Dragons             130
   3.3.1 Small Three Winds             30
   3.3.2 Big Three Winds               120
   3.3.3 Small four winds              320
   3.3.4 Big four winds                400
   3.4   All Honors                    320

4.0 Triplets and kong

   4.1   All triplets                  30
   4.2.1 Two Concealed Triplets        5
   4.2.2 Three Concealed Triplets      30
   4.2.3 Four concealed Triplets       125
   4.3.1 One kong                      5
   4.3.2 Two kong                      20
   4.3.3 Three kong                    120
   4.3.4 Four kong                     480

5.0 Identical sets

   5.1.1 Two identical sequences       10
   5.1.2 Two identical sequences twice 60
   5.1.3 Three identical sequences     120
   5.1.4 Four identical sequences      480

6.0 Similar sets

   6.1   Tree simila sequences         35
   6.2.1 Small three similar Triplets  30
   6.2.2 Three similar Triplets        120

7.0 Consecutive sets

   7.1   Nine-tile Straight            40
   7.2.1 Tree consecutive triplets     100
   7.2.2 Four consecutive triplets     200

8.0 Terminals

   8.1.1 Mixed lesser terminals        40
   8.1.2 Pure lesser terminals         50
   8.1.3 Mixed greater terminals       100
   8.1.4 Pure greater terminals        400

9.0 Incidental bonuses

   9.1.1 Final draw                    10
   9.1.2 Final discard                 10
   9.2   Win on kong                   10
   9.3   Robbing a kong                10
   9.4.1 Blessing of heaven            155
   9.4.2 Blessing of hearth            155

10.0 Irregular hands

   10.1  Thirteen terminals             160
   10.2  Seven Pairs                    30
   
   
*)

open Rule_manager

let trivial_patterns = flag "Trivial patterns"
let one_suit_patterns = flag "One-Suit Patterns"
let honor_tiles = flag "Honor tiles"


let flags = [trivial_patterns; one_suit_patterns; honor_tiles]

let default_flags = flags

let fold_declared_tilesets f init declared =
  List.fold_left
    (fun acc (tileset, _, concealed) ->
       f acc tileset concealed
    )
    init
    declared

let fold_mahjong_tilesets f init = function
  | Tileset.Regular tilesets ->
    List.fold_left
      (fun acc basic_tileset ->
         f acc (Tileset.tileset_of_basic_tileset basic_tileset) true
      )
      init
      tilesets
  | Tileset.Irregular _ -> init

let fold_tilesets f init mahjong declared =
  let acc = fold_declared_tilesets f init declared in
  fold_mahjong_tilesets f acc mahjong

let is_chow tileset =
  Tileset.is_chow tileset || Tileset.is_pair tileset

let all_chows mahjong declared =
  fold_tilesets
    (fun acc tileset _ ->
       if acc then
         is_chow tileset
       else
         false
    )
    true
    mahjong
    declared

let all_chows_pts mahjong declared =
  if all_chows mahjong declared then 5. else 0.

let concealed_hand mahjong declared =
  fold_tilesets
    (fun acc _ concealed -> acc && concealed)
    true
    mahjong
    declared

let concealed_hand_pts mahjong declared = 
  if concealed_hand mahjong declared then 5. else 0.

let no_terminal_tile tile_descr =
  let open Tileset in
  match tile_descr with
  | Bam (1 | 9)
  | Dot (1 | 9)
  | Char (1 | 9) -> false
  | _ -> true

let no_terminals_tileset tileset =
  List.for_all no_terminal_tile (Tileset.tile_descr_of_tileset tileset)

let no_terminals mahjong declared =
  fold_tilesets
    (fun acc tileset _ -> 
       if acc then
         no_terminals_tileset tileset
       else
         false
    )
    true
    mahjong
    declared

let no_terminals_pts mahjong declared =
  if no_terminals mahjong declared then 5. else 0.

let trivial_patterns_pts check mahjong declared =
  if check trivial_patterns then
    all_chows_pts mahjong declared +.
    concealed_hand_pts mahjong declared +.
    no_terminals_pts mahjong declared
  else
    0.

let suit_of_tileset tileset =
  let open Tileset in
  match List.hd (Tileset.tile_descr_of_tileset tileset) with
  | Bam _ -> `Bam
  | Dot _ -> `Dot
  | Char _ -> `Char
  | _ -> `Honor

let check_mixed s1 s2 =
  if s2 = `Honor || s1 = s2 then
    s1
  else
    `Fail

let check_pure s1 s2 =
  if s1 = s2 then s1 else `Fail

let one_suit_ check mahjong declared =
  let result =
    fold_tilesets
      (fun acc tileset _ ->
        match acc with
        | `Init -> suit_of_tileset tileset
        | `Fail -> `Fail
        | suit -> check suit (suit_of_tileset tileset)
      )
      `Init
      mahjong
      declared
  in
  match result with
  | `Init | `Fail | `Honor -> false
  | `Bam | `Dot | `Char -> true


let mixed_one_suit mahjong declared = one_suit_ check_mixed mahjong declared

let pure_one_suit mahjong declared = one_suit_ check_pure mahjong declared

let char_9_gates = Tileset.(tileset_of_tiles [c1; c1; c1; c2; c3; c4; c5; c6; c7; c8; c9; c9; c9])

let bam_9_gates = Tileset.(tileset_of_tiles [b1; b1; b1; b2; b3; b4; b5; b6; b7; b8; b9; b9; b9])

let dot_9_gates = Tileset.(tileset_of_tiles [d1; d1; d1; d2; d3; d4; d5; d6; d7; d8; d9; d9; d9])


let nine_gates_pts check last_tile hand =
  if check one_suit_patterns then
    let hand_before_win = Tileset.remove_tile last_tile hand in
    if hand_before_win = char_9_gates ||
       hand_before_win = dot_9_gates ||
       hand_before_win = bam_9_gates
    then
      480.
    else
      0.
  else
    0.

let one_suit_pts mahjong declared =
  if pure_one_suit mahjong declared then
    80.
  else if mixed_one_suit mahjong declared then
    40.
  else 0.

let one_suit_patterns_pts check mahjong declared =
  if check one_suit_patterns then
    one_suit_pts mahjong declared
  else
    0.

let value_honor_pts seat_wind mahjong declared =
  fold_tilesets
    (fun acc tileset _ ->
       let open Tileset in
       if is_pong tileset || is_kong tileset then
         let tile = List.hd (tiles_of_tileset tileset) in
         if tile = rd || tile = gd || tile = wd || tile = seat_wind then
           acc +. 10.
         else
           acc
       else
         acc
    )
    0.
    mahjong
    declared

let big_three_dragons _ _ = false

let small_three_dragons _ _ = false

let small_three_winds _ _ = false

let big_three_winds _ _ = false

let small_four_winds _ _ = false

let big_four_winds _ _ = false

let all_honor_pts _ _ = 0.

let three_winds_pts mahjong declared =
  if big_three_winds mahjong declared then
    120.
  else if small_three_winds mahjong declared then
    30.
  else
    0.

let four_winds_pts mahjong declared =
  if big_four_winds mahjong declared then
    400.
  else if small_four_winds mahjong declared then
    320.
  else
    0.

let three_dragons_pts mahjong declared =
  if big_three_dragons mahjong declared then
    130.
  else if small_three_dragons mahjong declared then
    40.
  else
    0.

let honor_tiles_pts check seat_wind mahjong declared =
  if check honor_tiles then
    value_honor_pts seat_wind mahjong declared +.
    three_dragons_pts mahjong declared +.
    three_winds_pts mahjong declared
  else
    0.

let honor_tiles_limits_pts check mahjong declared =
  if check honor_tiles then
    let four_winds_pts = four_winds_pts mahjong declared in
    if four_winds_pts = 0. then
      all_honor_pts mahjong declared
    else
      four_winds_pts
  else
    0.

let limit_hand_pts check last_tile hand mahjong declared =
  nine_gates_pts check last_tile hand +.
  honor_tiles_limits_pts check mahjong declared

let mahjong_pts check seat_wind last_tile hand mahjong declared =
  let limit_pts = limit_hand_pts check last_tile hand mahjong declared in
  if limit_pts = 0. then
    let pts =
      trivial_patterns_pts check mahjong declared +.
      one_suit_patterns_pts check mahjong declared +.
      honor_tiles_pts check seat_wind mahjong declared
    in
    min (max pts 1.) 320.
  else
    limit_pts

let payoff ~irregular_hands ~seven_pairs check player game =
  let open Engine in
  let points =
    match finished game with
    | None -> assert false
    | Some No_winner -> 0.
    | Some (Mahjong {declared; hand; _}) ->
      let nb_declared = List.length declared in
      let mahjongs =
        Tileset.mahjong ~irregular_hands ~seven_pairs (4 - nb_declared) hand
      in
      List.fold_left
        (fun acc mahjong ->
           max acc (mahjong_pts check (current_player_wind game) (last_tile game) hand mahjong declared)
        )
        0.
        mahjongs
  in
  if current_player game = player then
    points *. 3.
  else if points <= 25. then
    -. points
  else if discard_player game = Some player then
    -. (points *. 3. -. 50.)
  else
    -. 25.

let build_rule check =
  let irregular_hands = Tileset.no_irregular_hand in
  let seven_pairs = false (*TODO*) in
  let evaluate_game player game =
    payoff ~irregular_hands ~seven_pairs check player game
  in
  {irregular_hands; seven_pairs; evaluate_game}
      


let register () =
  register_rule_builder
    ~is_default: true
    ~flags
    ~default_flags
    ~build_rule
    "Zung Jung"
