(*Copyright (C) 2014 Denis Berthod*)

open Tileset
open Engine

let tiles =
  [|
    d1; d2; d3; d4; d5; d6; d7; d8; d9;
    b1; b2; b3; b4; b5; b6; b7; b8; b9;
    c1; c2; c3; c4; c5; c6; c7; c8; c9;
    rd; gd; wd; ew; sw; nw; ww
  |]

let random_hands nb =
  Random.self_init ();
  let hand () =
    let cpts = Array.make 34 0 in
    let rec hand n acc =
      if n = 0 then acc else
        let index = Random.int 34 in
        if cpts.(index) = 4 then hand n acc
        else begin
          cpts.(index) <- cpts.(index) + 1;
          hand (n - 1) (add_tile tiles.(index) acc)
        end
    in
    hand 14 empty
  in
  let rec aux acc n =
    if n = 0 then acc else
      aux (hand () :: acc) (n - 1)
  in
  aux [] nb

let test hands =
  List.fold_left
    (fun acc hand ->
      match mahjong ~seven_pairs: false 4 hand with
      | [] -> acc
      | x -> x @ acc
    )
    []
    hands

let () =
  let nb_hands = 1_000 in
  let hands = random_hands nb_hands in
  let ti = Unix.gettimeofday () in
  ignore (test hands);
  let tf = Unix.gettimeofday () in
  print_endline (Printf.sprintf "%i mahjong tests in %.4f seconds." nb_hands (tf -. ti))


let perf_test () =
  let nb_games = ref 0 in
  let nb_draw_games = ref 0 in
  let evaluate_game player game =
    incr nb_games;
    match finished game with
    | None -> assert false
    | Some No_winner -> incr nb_draw_games; 0.
    | Some (Mahjong _) ->
      if current_player game = player then
        1.
      else
        -1.
  in
  let nb_simulations = 1 in
  let nb_trajectory = 200 in
  let ti = Unix.gettimeofday () in
  let events =
    [
      Init (Array.make 136 None);
      Wall_breaker_roll 3;
      Break_wall_roll 11;
      Deal;
      Draw 0;
    ]
  in
  for _ = 1 to nb_simulations do
    ignore (Mahjong_ai.mc_ai_with_bias ~seven_pairs: false ~evaluate_game ~nb_trajectory  events 0.8)
  done;
  let tf = Unix.gettimeofday () in
  (tf -. ti), nb_simulations, nb_trajectory, !nb_draw_games, !nb_games 

let () =
  let time, nb_simulations, nb_trajectory, nb_draw_games, nb_games =
    perf_test ()
  in
  print_endline (Printf.sprintf "%i simulation with %i trajectory in %.4f seconds." nb_simulations nb_trajectory time);
  print_endline (Printf.sprintf "%i / %i draw games i.e. %.2f%%." nb_draw_games nb_games ((float nb_draw_games) /. (float nb_games) *. 100.))
