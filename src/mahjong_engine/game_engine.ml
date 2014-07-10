(* Copyright (C) 2014 Denis Berthod*)

open Tileset
open Engine

type game =
  {
    tiles: tile array;
    wall_breaker_roll: int;
    current_tile: int;
  }

let init_game =
  {
    tiles = [||];
    wall_breaker_roll = 0;
    current_tile = 0;
  }

let init_tiles =
  [| b1; b1; b1; b1; b2; b2; b2; b2; b3; b3; b3; b3;
     b4; b4; b4; b4; b5; b5; b5; b5; b6; b6; b6; b6;
     b7; b7; b7; b7; b8; b8; b8; b8; b9; b9; b9; b9;

     d1; d1; d1; d1; d2; d2; d2; d2; d3; d3; d3; d3;
     d4; d4; d4; d4; d5; d5; d5; d5; d6; d6; d6; d6;
     d7; d7; d7; d7; d8; d8; d8; d8; d9; d9; d9; d9;

     c1; c1; c1; c1; c2; c2; c2; c2; c3; c3; c3; c3;
     c4; c4; c4; c4; c5; c5; c5; c5; c6; c6; c6; c6;
     c7; c7; c7; c7; c8; c8; c8; c8; c9; c9; c9; c9;

     gd; gd; gd; gd; rd; rd; rd; rd; wd; wd; wd; wd;

     ww; ww; ww; ww; nw; nw; nw; nw;
     ew; ew; ew; ew; sw; sw; sw; sw
  |]

let nb_tiles = Array.length init_tiles

let random_game = Array.make nb_tiles (None: tile_descr option)

let init_tile_descrs = Array.map tile_descr_of_tile init_tiles

let find_index tile_descr tile_descrs =
  let i = ref 0 in
  let l = Array.length tile_descrs in
  while !i < l && tile_descrs.(!i) <> tile_descr do incr i done;
  !i

let find_free_backward i known_positions =
  let i = ref i in
  while 0 <= !i && known_positions.(!i) <> None do decr i done;
  if !i = (-1) then
    None
  else
    Some !i

let find_free_forward i known_positions =
  let i = ref i in
  let l = Array.length known_positions in
  while !i < l && known_positions.(!i) <> None do incr i done;
  if !i = l then
    None
  else
    Some !i

let find_swap_index i known_positions tile_descrs =
  match known_positions.(i) with
  | Some tile_desc -> find_index tile_desc tile_descrs
  | None ->
    let j = Random.int (i + 1) in
    match known_positions.(j) with
    | None -> j
    | Some _ ->
      match find_free_backward j known_positions with
      | Some k -> k
      | None ->
        match find_free_forward j known_positions with
        | Some k -> k
        | None -> assert false

let swap arr i j =
  let x = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- x

let shuffle known_positions =
  let tiles = Array.copy init_tiles in
  let tile_descrs = Array.copy init_tile_descrs in
  assert (Array.length tiles = Array.length known_positions);
  Random.self_init ();
  for i = Array.length tiles - 1 downto 0 do
    let j = find_swap_index i known_positions tile_descrs in
    swap tiles i j;
    swap tile_descrs i j
  done;
  tiles

let on_game_start_exit event game =
  match event with
  | Init known_positions -> {game with tiles = shuffle known_positions}
  | _ -> assert false

let on_wait_for_wall_breaker_roll_exit event game =
  match event with
  | Wall_breaker_roll wall_breaker_roll -> {game with wall_breaker_roll}
  | _ -> assert false



let run_game =
  build_engine
    ~on_game_start_exit
    ~on_wait_for_wall_breaker_roll_exit
    ()
