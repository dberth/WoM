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

   6.1   Three similar sequences       35
   6.2.1 Small three similar Triplets  30
   6.2.2 Three similar Triplets        120

7.0 Consecutive sets

   7.1   Nine-tile Straight            40
   7.2.1 Three consecutive triplets     100
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
let pong_and_kong = flag "Pong and Kong"
let identical_chow = flag "Identical Chow"
let similar_sets = flag "Similar Sets"
let consecutive_sets = flag "Consecutive Sets"
let terminals = flag "Terminals"


let flags = [trivial_patterns; one_suit_patterns; honor_tiles; pong_and_kong; identical_chow; similar_sets; consecutive_sets; terminals]

let default_flags = flags

let (@+) (explanations1, score1) (explanations2, score2) =
  explanations1 @ explanations2, score1 +. score2

let no_pts = [], 0.

let pts expl pt = [expl, pt], pt

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
  if all_chows mahjong declared then
    pts "All Chows"  5.
  else
    no_pts

let concealed_hand mahjong declared =
  fold_tilesets
    (fun acc _ concealed -> acc && concealed)
    true
    mahjong
    declared

let concealed_hand_pts mahjong declared = 
  if concealed_hand mahjong declared then
    pts "Concealed Hand" 5.
  else
    no_pts

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
  if no_terminals mahjong declared then
    pts "No Terminals" 5.
  else
    no_pts

let trivial_patterns_pts check mahjong declared =
  if check trivial_patterns then
    all_chows_pts mahjong declared @+
    concealed_hand_pts mahjong declared @+
    no_terminals_pts mahjong declared
  else
    no_pts

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
  if check one_suit_patterns then begin
    match last_tile with
    | None -> no_pts
    | Some last_tile ->
      try
        let hand_before_win = Tileset.remove_tile last_tile hand in
        if hand_before_win = char_9_gates ||
           hand_before_win = dot_9_gates ||
           hand_before_win = bam_9_gates
        then
          pts "Nine Gates" 480.
        else
          no_pts
      with
      | Not_found ->
        print_endline (String.concat "; " (List.map Tileset.string_of_tile (Tileset.tiles_of_tileset hand)));
        print_endline (Tileset.string_of_tile last_tile);
        assert false
  end else
    no_pts

let one_suit_pts mahjong declared =
  if pure_one_suit mahjong declared then
    pts "Pure One-Suit" 80.
  else if mixed_one_suit mahjong declared then
    pts "Mixed One-Suit" 40.
  else no_pts

let one_suit_patterns_pts check mahjong declared =
  if check one_suit_patterns then
    one_suit_pts mahjong declared
  else
    no_pts


let value_honor_pts seat_wind mahjong declared =
  fold_tilesets
    (fun acc tileset _ ->
       let open Tileset in
       if is_pong tileset || is_kong tileset then
         let tile = List.hd (tiles_of_tileset tileset) in
         if tile = rd || tile = gd || tile = wd || tile = seat_wind then
           pts "Value Honor" 10. @+ acc
         else
           acc
       else
         acc
    )
    no_pts
    mahjong
    declared

let honor_sets check_tileset mahjong declared =
  fold_tilesets
    (fun (nb_sets, nb_pairs) tileset _ ->
       let open Tileset in
       if is_pong tileset || is_kong tileset then
         if check_tileset tileset then
           nb_sets + 1, nb_pairs
         else
           nb_sets, nb_pairs
       else if is_pair tileset then
         if check_tileset tileset then
           nb_sets, nb_pairs + 1
         else
           nb_sets, nb_pairs
       else
         nb_sets, nb_pairs
    )
    (0, 0)
    mahjong
    declared


let check_dragon tileset =
  let open Tileset in
  let tile = List.hd (tiles_of_tileset tileset) in
  tile = rd || tile = gd || tile = wd

let check_wind tileset =
  let open Tileset in
  let tile = List.hd (tiles_of_tileset tileset) in
  tile = ew || tile = sw || tile = ww || tile = nw

let contains_honor tileset =
  let open Tileset in
  let tile = List.hd (tiles_of_tileset tileset) in
  tile = rd || tile = gd || tile = wd ||
  tile = ew || tile = sw || tile = ww || tile = nw

let all_honor mahjong declared =
  fold_tilesets
    (fun acc tileset _ ->
       let open Tileset in
       if acc then
         if is_chow tileset then
           false
         else
           contains_honor tileset
       else
         acc
    )
    true
    mahjong
    declared

let all_honor_pts mahjong declared =
  if all_honor mahjong declared then begin
    pts "All Honors" 320.
  end else
    no_pts
    

let three_winds_pts mahjong declared =
  match honor_sets check_wind mahjong declared with
  | 3, 0 -> pts "Big Three Winds" 120.
  | 2, 1 -> pts "Small Three Winds" 30.
  | _ -> no_pts

let four_winds_pts mahjong declared =
  match honor_sets check_wind mahjong declared with
  | 4, 0 -> pts "Big Four Winds" 400.
  | 3, 1 -> pts "Small Four Winds" 320.
  | _ -> no_pts

let three_dragons_pts mahjong declared =
  match honor_sets check_dragon mahjong declared with
  | 3, 0 -> pts "Big Three Dragons" 130.
  | 2, 1 -> pts "Small Three Dragons" 40.
  | _ -> no_pts

let honor_tiles_pts check seat_wind mahjong declared =
  if check honor_tiles then
    value_honor_pts seat_wind mahjong declared @+
    three_dragons_pts mahjong declared @+
    three_winds_pts mahjong declared
  else
    no_pts

let honor_tiles_limits_pts check mahjong declared =
  if check honor_tiles then
    let four_winds_pts = four_winds_pts mahjong declared in
    if snd four_winds_pts = 0. then
      all_honor_pts mahjong declared
    else
      four_winds_pts
  else
    no_pts

let is_pong tileset =
  Tileset.is_pong tileset || Tileset.is_kong tileset || Tileset.is_pair tileset

let all_pong mahjong declared =
  fold_tilesets
    (fun acc tileset _ ->
       if acc then
         is_pong tileset
       else
         false
    )
    true
    mahjong
    declared

let all_pong_pts mahjong declared =
  if all_pong mahjong declared then
    pts "All Pong and Kong" 30.
  else
    no_pts

let nb_concealed_pong mahjong declared =
  fold_tilesets
    (fun acc tileset concealed ->
       if concealed && (Tileset.is_pong tileset || Tileset.is_kong tileset) then
         acc + 1
       else
         acc
    )
    0
    mahjong
    declared


let nb_kong mahjong declared =
  fold_tilesets
    (fun acc tileset _ ->
       if Tileset.is_kong tileset then
         acc + 1
       else
         acc
    )
    0
    mahjong
    declared

let concealed_pong_pts mahjong declared =
  match nb_concealed_pong mahjong declared with
  | 0 | 1 -> no_pts
  | 2 -> pts "Two Concealed Pong or Kong" 5.
  | 3 -> pts "Three Concealed Pong or Kong" 30.
  | 4 -> pts "Four Concealed Pong or Kong" 125.
  | _ -> assert false

let several_kong_pts mahjong declared =
  match nb_kong mahjong declared with
  | 0 -> no_pts
  | 1 -> pts "One Kong" 5.
  | 2 -> pts "Two Kong" 20.
  | 3 -> pts "Three Kong" 120.
  | 4 -> no_pts
  | _ -> assert false


let four_kong check mahjong declared =
  if check pong_and_kong then
    if nb_kong mahjong declared = 4 then
      pts "Four Kong" 480.
    else
      no_pts
  else
    no_pts

let pong_and_kong_pts check mahjong declared =
  if check pong_and_kong then
    all_pong_pts mahjong declared @+
    concealed_pong_pts mahjong declared @+
    several_kong_pts mahjong declared
  else
    no_pts

let chow_partition mahjong declared =
  let partition = Hashtbl.create 4 in
  begin
    fold_tilesets
      (fun () tileset _ ->
         if Tileset.is_chow tileset then
           match Hashtbl.find partition tileset with
           | nb -> Hashtbl.replace partition tileset (nb + 1)
           | exception Not_found ->
             Hashtbl.add partition tileset 1
      )
      ()
      mahjong
      declared
  end;
  Hashtbl.fold (fun _ nb acc -> if 1 < nb then nb :: acc else acc) partition []

let identical_chow_pts check mahjong declared =
  if check identical_chow then
    match chow_partition mahjong declared with
    | [2] -> pts "Two Identical Chow" 10.
    | [2; 2] -> pts "Two Identical Chow Twice" 60.
    | [3] -> pts "Three Identical Chow" 120.
    | _ -> no_pts
  else
    no_pts

let four_identical_chow check mahjong declared =
  if check identical_chow then
    match chow_partition mahjong declared with
    | [4] -> pts "Four Identical Chow" 480.
    | _ -> no_pts
  else
    no_pts

let classify_set tileset =
  let open Tileset in
  let tile_descrs = List.sort compare (tile_descr_of_tileset tileset) in
  let kind =
    if is_kong tileset || is_pong tileset then
      `Pong
    else if is_chow tileset then
      `Chow
    else
      `Pair
  in
  match List.hd tile_descrs with
  | Bam i -> Some (i, `Bam, kind)
  | Dot i -> Some (i, `Dot, kind)
  | Char i -> Some (i, `Char, kind)
  | _ -> None

let classify_sets mahjong declared =
  fold_tilesets
    (fun acc tileset _ ->
       match classify_set tileset with
       | Some x -> x :: acc
       | None -> acc
    )
    []
    mahjong
    declared

let similar_series = List.sort compare [`Bam; `Char; `Dot]

let three_similar_chow mahjong declared =
  let table = Hashtbl.create 4 in
  List.iter
    (fun (i, serie, kind) ->
       match kind with
       | `Chow ->
         begin match Hashtbl.find table i with
         | series -> Hashtbl.replace table i (serie :: series)
         | exception Not_found -> Hashtbl.add table i [serie]
         end
       | _ -> ()
    )
    (classify_sets mahjong declared);
  Hashtbl.fold
    (fun _ series acc ->
       if acc then
         true
       else
         List.sort compare series = similar_series
    )
    table
    false
  

let three_similar_chow_pts mahjong declared =
  if three_similar_chow mahjong declared then
    pts "Three Similar Chow" 35.
  else
    no_pts

let similar_pong_or_kong mahjong declared =
  let table = Hashtbl.create 4 in
  let pair = ref None in
  List.iter
    (fun (i, serie, kind) ->
       match kind with
       | `Pair -> pair := Some (i, serie)
       | `Pong ->
         begin match Hashtbl.find table i with
         | series -> Hashtbl.replace table i (serie :: series)
         | exception Not_found -> Hashtbl.add table i [serie]
         end
       | `Chow -> ()
    )
    (classify_sets mahjong declared);
  Hashtbl.fold
    (fun i series result ->
       match result with
       | `Big | `Small -> result
       | `None ->
         let series = List.sort compare series in
         if series = similar_series then
           `Big
         else
           match !pair with
           | Some (i_pair, pair_serie) when i_pair = i ->
             if List.sort compare (pair_serie :: series) = similar_series then
               `Small
             else
               `None
           | _ -> `None
    )
    table
    `None

let similar_pong_or_kong_pts mahjong declared =
  match similar_pong_or_kong mahjong declared with
  | `Small -> pts "Small Similar Pong or Kong" 30.
  | `Big -> pts "Similar Pong or Kong" 120.
  | `None -> no_pts


let similar_sets_pts check mahjong declared =
  if check similar_sets then
    three_similar_chow_pts mahjong declared @+
    similar_pong_or_kong_pts mahjong declared
  else
    no_pts

let is_straight l =
  List.mem 1 l && List.mem 4 l && List.mem 7 l

let nine_tile_straight mahjong declared =
  let chars, bams, dots =
    fold_tilesets
      (fun (chars, bams, dots) tileset _ ->
         let open Tileset in
         if is_chow tileset then
           let tile = List.hd (List.sort compare (tile_descr_of_tileset tileset)) in
           match tile with
           | Bam i -> chars, i :: bams, dots
           | Dot i -> chars, bams, i :: dots
           | Char i -> i :: chars, bams, dots
           | _ -> chars, bams, dots
         else
           (chars, bams, dots)
      )
      ([], [], [])
      mahjong
      declared
  in
  List.fold_left
    (fun result l -> if result then true else is_straight l)
    false
    [chars; bams; dots]


let nine_tile_straight_pts mahjong declared =
  if nine_tile_straight mahjong declared then
    pts "Nine-Tile Straight" 40.
  else
    no_pts

let nb_consecutive l =
  let result, _, _ =
    List.fold_left
      (fun (result, cur_max, prec) i ->
         let cur_max =
           if i = prec + 1 then
             cur_max + 1
           else
             1
         in
         (max result cur_max, cur_max, i)
      )
      (0, 0, -1)
      l
  in
  result


let consecutive_pong_or_kong mahjong declared =
  let sets = classify_sets mahjong declared in
  let table = Hashtbl.create 4 in
  List.iter
    (fun (i, serie, kind) ->
       match kind with
       | `Pong | `Kong -> begin
           match Hashtbl.find table serie with
           | l -> Hashtbl.replace table serie (i :: l)
           | exception Not_found -> Hashtbl.add table serie [i]
         end
       | _ -> ()
    )
    sets;
  Hashtbl.fold
    (fun _ l result -> max result (nb_consecutive (List.sort compare l)))
    table
    0

let consecutive_pong_or_kong_pts mahjong declared =
  match consecutive_pong_or_kong mahjong declared with
  | 3 -> pts "Three Consecutive Pong or Kong" 100.
  | 4 -> pts "Four Consecutive Pong or Kong" 200.
  | _ -> no_pts

let consecutive_sets_pts check mahjong declared =
  if check consecutive_sets then
    nine_tile_straight_pts mahjong declared @+
    consecutive_pong_or_kong_pts mahjong declared
  else
    no_pts

let contains_terminal tileset =
  let open Tileset in
  let tile = List.hd (tiles_of_tileset tileset) in
  let res =
    tile = d1 || tile = c1 || tile = b1 ||
    tile = d9 || tile = c9 || tile = b9
  in
  if res then
    res
  else
    if tile = d7 || tile = c7 || tile = b7 then
      Tileset.is_chow tileset
    else
      false

let all_terminals tileset =
  let open Tileset in
  if not (Tileset.is_chow tileset) then
    let tile = List.hd (tiles_of_tileset tileset) in
    tile = d1 || tile = d9 || tile = b1 || tile = b9 ||
    tile = c1 || tile = c9
  else
    false

let mixed_lesser_terminals mahjong declared =
  fold_tilesets
    (fun result tileset _ ->
       if result then
         contains_honor tileset || contains_terminal tileset
       else
         false
    )
    true
    mahjong
    declared

let pure_lesser_terminals mahjong declared =
  fold_tilesets
    (fun result tileset _ ->
       if result then
         contains_terminal tileset
       else
         false
    )
    true
    mahjong
    declared

let mixed_greater_terminals mahjong declared =
  fold_tilesets
    (fun result tileset _ ->
       if result then
         contains_honor tileset || all_terminals tileset
       else
         false
    )
    true
    mahjong
    declared

let pure_greater_terminals mahjong declared =
  fold_tilesets
    (fun result tileset _ ->
       if result then
         all_terminals tileset
       else
         false
    )
    true
    mahjong
    declared


let terminals_pts mahjong declared =
  if mixed_greater_terminals mahjong declared then
    pts "Mixed Greater Terminals" 100.
  else if pure_lesser_terminals mahjong declared then
    pts "Pure Lesser Terminals" 50.
  else if mixed_lesser_terminals mahjong declared then
    pts "Mixed Lesser Terminals" 40.
  else
    no_pts

let terminals_pts check mahjong declared =
  if check terminals then
    terminals_pts mahjong declared
  else
    no_pts

let pure_greater_terminals check mahjong declared =
  if check terminals then
    if pure_greater_terminals mahjong declared then
      pts "Pure Greater Terminals" 400.
    else
      no_pts
  else
    no_pts

let limit_hand_pts check last_tile hand mahjong declared =
  nine_gates_pts check last_tile hand @+
  honor_tiles_limits_pts check mahjong declared @+
  four_kong check mahjong declared @+
  four_identical_chow check mahjong declared @+
  pure_greater_terminals check mahjong declared

let mahjong_pts check seat_wind last_tile hand mahjong declared =
  let limit_pts = limit_hand_pts check last_tile hand mahjong declared in
  if snd limit_pts = 0. then
    let points =
      trivial_patterns_pts check mahjong declared @+
      one_suit_patterns_pts check mahjong declared @+
      honor_tiles_pts check seat_wind mahjong declared @+
      pong_and_kong_pts check mahjong declared @+
      identical_chow_pts check mahjong declared @+
      similar_sets_pts check mahjong declared @+
      consecutive_sets_pts check mahjong declared @+
      terminals_pts check mahjong declared
    in
    if snd points = 0. then
      pts "Chicken Hand" 1.
    else if 320. < snd points then
      fst points, 320.
    else
      points
  else
    limit_pts

let player_pts status hand_pts =
  match status with    
    | `Winner -> "Winner receives 3 x hand points", 3. *. hand_pts
    | `Draw -> "Draw", 0.
    | (`Looser | `Discarder | `Big_looser as t) ->
      if hand_pts <= 25. then
        "Looser to a small hand pays hand points", -. hand_pts
      else
        match t with
        | `Looser -> "Looser to a big hand pays 25 points", -. 25.
        | `Big_looser -> "Looser to a self drawn hand pays hand points", -. hand_pts
        | `Discarder -> "Discarder to a big hand pays hand points x 3 - 50", -. (hand_pts *. 3. -. 50.)
        

let explain_hand_score ~irregular_hands ~seven_pairs check game =
  let open Engine in
  match finished game with
  | None -> assert false
  | Some No_winner -> [], 0.
  | Some (Mahjong {declared; hand; last_drawn_tile; _}) ->
      let nb_declared = List.length declared in
      let mahjongs =
        Tileset.mahjong ~irregular_hands ~seven_pairs (4 - nb_declared) hand
      in
      List.fold_left
        (fun (explanations, score) mahjong ->
           let new_explanations, new_score =
             mahjong_pts check (current_player_wind game) last_drawn_tile hand mahjong declared
           in
           if new_score < score then
             explanations, score
           else
             new_explanations, new_score
        )
        ([], 0.)
        mahjongs

let explain_player_score player game ~hand_score =
  let open Engine in
  let status =
    if hand_score = 0. then
      `Draw
    else if current_player game = player then
      `Winner
    else
      match finished game with
      | Some (Mahjong {discard_player; _}) -> begin
        match discard_player with
        | None -> `Big_looser
        | Some discard_player ->
        if discard_player = player then
          `Discarder
        else
          `Looser
        end
      | _ -> assert false
  in
  player_pts status hand_score


let build_rule check =
  let irregular_hands = Tileset.no_irregular_hand in
  let seven_pairs = false (*TODO*) in
  let explain_hand_score game =
    explain_hand_score ~irregular_hands ~seven_pairs check game
  in
  let evaluate_game player game =
    let hand_score = snd (explain_hand_score game) in
    snd (explain_player_score player game ~hand_score)
  in
  {irregular_hands; seven_pairs; evaluate_game; explain_hand_score; explain_player_score}
      


let register () =
  register_rule_builder
    ~is_default: true
    ~flags
    ~default_flags
    ~build_rule
    "Zung Jung"
