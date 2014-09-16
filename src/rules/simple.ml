(*Copyright (C) 2014 Denis Berthod*)

open Rule_manager 

let all_chows = flag "All chows"

let all_pongs = flag "All pongs"

let seven_pairs = flag "Seven pairs"

let flags = [all_chows; all_pongs; seven_pairs]

let default_flags = flags

let is_chow tileset =
  Tileset.is_chow tileset || Tileset.is_pair tileset

let is_declared_chow (tileset, _, _) = is_chow tileset

let is_pong tileset =
  Tileset.is_pong tileset || Tileset.is_kong tileset || Tileset.is_pair tileset

let is_declared_pong (tileset, _, _) = is_pong tileset

let all_chows_pts check mahjong declared =
  if check all_chows then
    if List.for_all is_declared_chow declared then
      match mahjong with
      | Tileset.Regular tilesets ->
        if List.for_all (fun x -> is_chow (Tileset.tileset_of_basic_tileset x)) tilesets then
          2.
        else
          0.
      | Tileset.Irregular _ -> 0.
    else
      0.
  else
    0.

let all_pongs_pts check mahjong declared =
  if check all_pongs then
    if List.for_all is_declared_pong declared then
      match mahjong with
      | Tileset.Regular tilesets ->
        if List.for_all (fun x -> is_pong (Tileset.tileset_of_basic_tileset x)) tilesets then
          3.
        else
          0.
      | Tileset.Irregular _ -> 0.
    else
      0.
  else
    0.

let seven_pairs_pts check mahjong declared =
  if check seven_pairs then
    if declared = [] then
      match mahjong with
      | Tileset.Regular tilesets ->
        if List.length tilesets = 7 then
          15.
        else
          0.
      | Tileset.Irregular _ -> 0.
    else
      0.
  else
    0.

let mahjong_pts check mahjong declared =
  let special_pts =
    all_chows_pts check mahjong declared +.
    all_pongs_pts check mahjong declared +.
    seven_pairs_pts check mahjong declared
  in
  max special_pts 1.

let build_rule check =
  let open Engine in
  let irregular_hands = Tileset.no_irregular_hand in
  let evaluate_game player game =
    let points =
      match finished game with
      | None -> assert false
      | Some No_winner -> 0.
      | Some (Mahjong {declared; hand; _}) ->
        let nb_declared = List.length declared in
        let mahjongs = Tileset.mahjong ~irregular_hands ~seven_pairs: false (4 - nb_declared) hand in
        List.fold_left
          (fun acc mahjong ->
            max acc (mahjong_pts check mahjong declared)
          )
          0.
          mahjongs
    in
    if current_player game = player then
      points
    else
      -. points
  in
  {irregular_hands; seven_pairs = false; evaluate_game}

let register () =
  register_rule_builder
    ~is_default: true
    ~flags
    ~default_flags
    ~build_rule
    "Basic"
