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

let flags = [trivial_patterns]

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

let one_suit_patterns = flag "One-Suit Patterns"

let mixed_one_suit_pts _mahjong _declared = 0. (*TODO*)

let pure_one_suit_pts _mahjong _declrared = 0. (*TODO*)

let nine_gates_pts _mahjong _declared = 0. (*TODO*)

let one_suit_pts mahjong declared =
  match pure_one_suit_pts mahjong declared with
  | 0. -> mixed_one_suit_pts mahjong declared
  | x -> x

let one_suit_patterns_pts check mahjong declared =
  if check one_suit_patterns then
    one_suit_pts mahjong declared +.
    nine_gates_pts mahjong declared
  else
    0.

let mahjong_pts check mahjong declared =
  let pts =
    trivial_patterns_pts check mahjong declared +.
    one_suit_patterns_pts check mahjong declared
  in
  max pts 1.

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
           max acc (mahjong_pts check mahjong declared)
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
