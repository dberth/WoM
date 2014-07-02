(* Copyright (C) 2014 Denis Berthod *)

open OUnit2
open Tileset

let regular_mahjong ctx =
  todo "Regular mahjong";
  assert_equal 1 1

let tileset_test_suite =
  "Tileset test suite" >:::
  [
   "Tile description" >:: (fun ctx -> assert_equal (tile_descr_of_tile b1) (Bam 1));
   "Regular mahjong" >:: regular_mahjong
 ]

let engine_suite =
  "Mahjong engine test suite" >:::
  [
   tileset_test_suite
 ]

let () = run_test_tt_main engine_suite
    
