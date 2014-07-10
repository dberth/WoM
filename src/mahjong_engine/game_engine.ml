(* Copyright (C) 2014 Denis Berthod*)

open Tileset
open Engine

type game =
    {
     tiles: tile array;
   }

let init_game =
  {
   tiles = [||];
 }

let init_tiles =
  [| b1; b1; b1; b1; b1; b2; b2; b2; b2; b3; b3; b3; b3; b4; b4; b4; b5; b5; b5; b5; b6; b6; b6; b6; b7; b7; b7; b7; b7; b8; b8; b8; b8; b9; b9; b9; b9;
     d1; d1; d1; d1; d1; d2; d2; d2; d2; d3; d3; d3; d3; d4; d4; d4; d5; d5; d5; d5; d6; d6; d6; d6; d7; d7; d7; d7; d7; d8; d8; d8; d8; d9; d9; d9; d9;
     c1; c1; c1; c1; c1; c2; c2; c2; c2; c3; c3; c3; c3; c4; c4; c4; c5; c5; c5; c5; c6; c6; c6; c6; c7; c7; c7; c7; c7; c8; c8; c8; c8; c9; c9; c9; c9;
     gd; gd; gd; gd; rd; rd; rd; rd; wd; wd; wd; wd;
     ww; ww; ww; ww; nw; nw; nw; nw; nw; ew; ew; ew; ew; sw; sw; sw; sw
  |]

let shuffle x = x

let on_game_start_exit event game =
  match event with
  | Init None -> {tiles = shuffle init_tiles}
  | Init (Some tiles) -> {tiles}
  | _ -> assert false

let run_game =
  build_engine
    ~on_game_start_exit
    ()
