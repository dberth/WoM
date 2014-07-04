(*Copyright (C) 2014 Denis Berthod*)

open Tileset

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
        match mahjong 4 hand with
        | [] -> acc
        | x -> x @ acc
      )
      []
      hands

let () =
  let nb_hands = 1_000_000 in
  let hands = random_hands nb_hands in
  let ti = Unix.gettimeofday () in
  ignore (test hands);
  let tf = Unix.gettimeofday () in
  print_endline (Printf.sprintf "%i mahjong tests in %.4f seconds." nb_hands (tf -. ti))
