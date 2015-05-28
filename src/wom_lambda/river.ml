(*Copyright (C) 2015 Denis Berthod*)

open LTerm_widget
open LTerm_geom

let st_underline = {LTerm_style.none with LTerm_style.underline = Some true}

type player_river_content =
  {
    mutable seat_wind: string;
  }

let empty_player_content player =
  let seat_wind =
    match player with
    | 0 -> "E"
    | 1 -> "S"
    | 2 -> "W"
    | 3 -> "N"
    | _ -> assert false
  in
  {
    seat_wind;
  }

type wall_model =
  {
    mutable current_tile: int;
    (*0 is the upper tile of the rightmost stack before player 0 *)
    mutable last_tile: int;
    mutable nb_tiles_in_kong_box: int;
  }

(* let nb_tiles_in_wall player nb_tiles_in_game {current_tile; last_tile; nb_tiles_in_kong_box} = *)
(*   let max_tile_in_side_wall = nb_tiles_in_game / 4 in *)
(*   let sub_mod x y = (x + nb_tiles_in_game - y) mode nb_tiles_in_game in *)
(*   let rotate index = sub_mod index (player * max_tile_in_side_wall) in *)
(*   (\*Rotate to change referentiel: tile 0 is new the upper rightmost tile of the player.*\) *)
(*   let start_of_wall = rotate current_tile in *)
(*   let end_of_wall = rotate (sub_mod last_tile nb_tiles_in_kong_box) in *)
(*   let end_of_kong_box = rotate last_tile in *)
(*   let in_side x = x < max_tile_in_side_wall in *)
(*   match in_side start_of_wall, in_side end_of_wall, in_side end_of_kong_box, start_of_wall <= end_of_wall with *)
(*   | false, false, false, false -> 0, 0 *)
(*   | false, false, false, true -> max_tile_in_side_wall, 0 *)
(*   | true, false, false, _ -> max_tile_in_side_wall - start_of_wall, 0 *)
(*   | false, true, false, _ ->  *)

class river nb_tiles kind =
  let river_content = Array.init 4 (fun i -> empty_player_content i) in
  let nb_stacks_per_side = nb_tiles / 8 in
  object
    inherit t kind

    method width = nb_stacks_per_side + 8

    method! draw ctx _focused_widget =
      let river_rec = {row1 = 0; col1 = 0; row2 = nb_stacks_per_side + 4; col2 = nb_stacks_per_side + 8} in
      LTerm_draw.draw_frame ctx river_rec LTerm_draw.Heavy;
      let river_ctx = LTerm_draw.sub ctx river_rec in
      LTerm_draw.draw_string_aligned river_ctx 0 LTerm_geom.H_align_center (Printf.sprintf "  %s  " river_content.(2).seat_wind);
      LTerm_draw.draw_string ~style: st_underline ctx 1 4 "~~~~~~~~~~~~~~~~~";
      LTerm_draw.draw_string ctx 2 2 "ll                 ll";
      LTerm_draw.draw_string ctx 3 2 "ll                 ll";
      LTerm_draw.draw_string ctx 4 2 "ll                 ll";
      LTerm_draw.draw_string ctx 5 2 "ll                 ll";
      LTerm_draw.draw_string ctx 6 2 "ll                 ll";
      LTerm_draw.draw_string ctx 7 2 "ll                 ll";
      LTerm_draw.draw_string ctx 8 2 "ll                 ll";
      LTerm_draw.draw_string ctx 9 0 "  ll                 ll  ";
      LTerm_draw.draw_string ctx 10 0
        (Printf.sprintf "%s ll                 ll %s" river_content.(3).seat_wind river_content.(1).seat_wind);
      LTerm_draw.draw_string ctx 11 0 "  ll                 ll  ";
      LTerm_draw.draw_string ctx 12 2 "ll                 ll";
      LTerm_draw.draw_string ctx 13 2 "ll                 ll";
      LTerm_draw.draw_string ctx 14 2 "ll                 ll";
      LTerm_draw.draw_string ctx 15 2 "ll                 ll";
      LTerm_draw.draw_string ctx 16 2 "ll                 ll";
      LTerm_draw.draw_string ctx 17 2 "ll                 ll";
      LTerm_draw.draw_string ctx 18 2 "ll                 ll";
      LTerm_draw.draw_string ~style: st_underline ctx 19 4 "~~~~~~~~~~~~~~~~~~";
      LTerm_draw.draw_string_aligned river_ctx 20 LTerm_geom.H_align_center (Printf.sprintf "  %s  " river_content.(0).seat_wind);


  end
