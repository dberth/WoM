(*Copyright (C) 2014 Denis Berthod*)

open Rule_manager 

let all_chows = flag "All chows"

let all_pongs = flag "All pongs"

let seven_pairs = flag "Seven pairs"

let flags = [all_chows; all_pongs; seven_pairs]

let default_flags = flags

let (@+) (s1, f1) (explanations, score) =
  if f1 = 0. then
    explanations, score
  else
    (s1, f1) :: explanations, f1 +. score

let is_chow tileset =
  Tileset.is_chow tileset || Tileset.is_pair tileset

let is_declared_chow (tileset, _, _) = is_chow tileset

let is_pong tileset =
  Tileset.is_pong tileset || Tileset.is_kong tileset || Tileset.is_pair tileset

let is_declared_pong (tileset, _, _) = is_pong tileset

let no_pts = "", 0.

let all_chows_pts check mahjong declared =
  if check all_chows then
    if List.for_all is_declared_chow declared then
      match mahjong with
      | Tileset.Regular tilesets ->
        if List.for_all (fun x -> is_chow (Tileset.tileset_of_basic_tileset x)) tilesets then
          "All chows", 2.
        else
          no_pts
      | Tileset.Irregular _ -> no_pts
    else
      no_pts
  else
    no_pts

let all_pongs_pts check mahjong declared =
  if check all_pongs then
    if List.for_all is_declared_pong declared then
      match mahjong with
      | Tileset.Regular tilesets ->
        if List.for_all (fun x -> is_pong (Tileset.tileset_of_basic_tileset x)) tilesets then
          "All pongs", 3.
        else
          no_pts
      | Tileset.Irregular _ -> no_pts
    else
      no_pts
  else
    no_pts

let seven_pairs_pts check mahjong declared =
  if check seven_pairs then
    if declared = [] then
      match mahjong with
      | Tileset.Regular tilesets ->
        if List.length tilesets = 7 then
          "Seven pairs", 15.
        else
          no_pts
      | Tileset.Irregular _ -> no_pts
    else
      no_pts
  else
    no_pts

let mahjong_pts check mahjong declared =
  let special_pts =
    all_chows_pts check mahjong declared @+
    all_pongs_pts check mahjong declared @+
    seven_pairs_pts check mahjong declared @+
    ([], 0.)
  in
  if snd special_pts = 0. then
    ["Chicken hand", 1.], 1.
  else
    special_pts

let player_pts status hand_pts =
  match status with
  | `Winner -> "Winner receives 3 x hand points", 3. *. hand_pts
  | `Looser -> "Looser pays 1 x hand points", -. hand_pts
  | `Draw -> "Draw", 0.

let build_rule check =
  let open Engine in
  let irregular_hands = Tileset.no_irregular_hand in
  let seven_pairs = check seven_pairs in
  let explain_hand_score round =
    match finished round with
    | None -> assert false
    | Some No_winner -> [], 0.
    | Some (Mahjong {declared; hand; _}) ->
      let nb_declared = List.length declared in
      let mahjongs = Tileset.mahjong ~irregular_hands ~seven_pairs (4 - nb_declared) hand in
      List.fold_left
        (fun (explanations, score) mahjong ->
           let new_explanations, new_score = mahjong_pts check mahjong declared in
           if new_score < score then
             explanations, score
           else
             new_explanations, new_score
        )
        ([], 0.)
        mahjongs
      
  in
  let explain_player_score player round ~hand_score =
    let status =
      if hand_score = 0. then
        `Draw
      else if current_player round = player then
        `Winner
      else
        `Looser
    in
    player_pts status hand_score 
  in
  let evaluate_round player round =
    let hand_score = snd (explain_hand_score round) in
    snd (explain_player_score player round ~hand_score)
  in
  {irregular_hands; seven_pairs; evaluate_round; explain_hand_score; explain_player_score}

let register () =
  register_rule_builder
    ~is_default: false
    ~flags
    ~default_flags
    ~build_rule
    "Basic"
