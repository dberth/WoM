(* Copyright (C) 2014 Denis Berthod *)

open OUnit2

let regular_mahjong ctx =
  todo "Regular mahjong";
  assert_equal 1 1

let tileset_test_suite =
  "Tileset test suite" >:::
  [
   "Regular mahjong" >:: regular_mahjong
 ]

let engine_suite =
  "Mahjong engine test suite" >:::
  [
   tileset_test_suite
 ]

let () = run_test_tt_main engine_suite
    
