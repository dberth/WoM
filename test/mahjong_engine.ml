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
  ["Regular mahjong",
   [b1; b2; b3; c1; c2; c3; rd; rd; rd; ww; ww; ww; d1; d1],
   [[[Dot 1; Dot 1]; [West_wind; West_wind; West_wind]; [Red_dragon; Red_dragon; Red_dragon]; [Bam 1; Bam 2; Bam 3]; [Char 1; Char 2; Char 3]   ]];
 ] |>
 List.map
   (fun (title, tiles, descr) ->
     title >:: fun _ctx ->
       assert_equal ~printer: pp_mahjong_list descr (mahjong 4 (tileset_of_tiles tiles) |> List.map tile_descr_of_mahjong)
   )
 
  

let tileset_test_suite =
  "Tileset test suite" >:::
  [
   ("tile_descr_of_tile" >::: tile_description);
   ("mahjong" >::: mahjong);
 ]

let engine_suite =
  "Mahjong engine test suite" >:::
  [
   tileset_test_suite
 ]

let () = run_test_tt_main engine_suite
    
