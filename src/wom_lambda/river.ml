(*Copyright (C) 2015 Denis Berthod*)

open LTerm_widget
open LTerm_geom
open Common


type player_river_content =
  {
    mutable seat_wind: wind;
  }

let empty_player_content player =
  let seat_wind =
    match player with
    | 0 -> East
    | 1 -> South
    | 2 -> West
    | 3 -> North
    | _ -> assert false
  in
  {
    seat_wind;
  }

type wall_content =
  | Wall
  | Kong_box
  | Empty

let wall_content_at_index nb_tiles_in_game nb_tile_in_kong_box wall_start last_tile =
  let sub_mod x y = (x + nb_tiles_in_game - y) mod nb_tiles_in_game in
  let rotate x = sub_mod x (wall_start / 2 * 2) in
  let wall_start = rotate wall_start in
  let last_tile = rotate last_tile in
  let wall_end = last_tile - nb_tile_in_kong_box in
  let kong_start, kong_end =
    if (nb_tile_in_kong_box mod 2 = 0) = (last_tile mod 2 = 0) then
      wall_end + 4, last_tile + 3
    else
      wall_end + 3, last_tile + 2
  in
  (fun x ->
    let x = rotate x in
    if wall_start <= x && x <= wall_end then
      Wall
    else if kong_start <= x && x <= kong_end then
      Kong_box
    else
      Empty
  )

let st_kong_box = LTerm_style.({none with foreground = Some cyan})

let st_kong_box_und = LTerm_style.({st_kong_box with underline = Some true})

let st_wall = LTerm_style.({none with foreground = Some lblue})

let st_wall_und = LTerm_style.({st_wall with underline = Some true})

let strings_of_buffer buffer =
  let sbuffer = Buffer.create 16 in
  let rec aux acc cur_style index =
    if Array.length buffer <= index then
      begin match cur_style with
        | None -> acc
        | Some style -> List.rev ((Buffer.contents sbuffer, style) :: acc)
      end
    else
      let c, style = buffer.(index) in
      match cur_style with
      | None ->
        Buffer.add_char sbuffer c;
        aux acc (Some style) (index + 1)
      | Some cur_style ->
        if LTerm_style.equal cur_style style then begin
          Buffer.add_char sbuffer c;
          aux acc (Some style) (index + 1)
        end else begin
          let content = Buffer.contents sbuffer in
          Buffer.clear sbuffer;
          Buffer.add_char sbuffer c;
          aux ((content, cur_style) :: acc) (Some style) (index + 1)
        end
  in
  aux [] None 0

let draw_horizontal_row ctx pos wall_content nb_tiles_in_game =
  let buffer = Array.make (nb_tiles_in_game / 8) (' ', LTerm_style.none) in 
  for char_index = 0 to nb_tiles_in_game / 8 - 1 do
    let even_tile_index =
      match pos with
      | `Top -> nb_tiles_in_game / 2 + char_index * 2
      | `Bottom -> nb_tiles_in_game / 4 - char_index * 2
    in
    let x =
      match wall_content even_tile_index, wall_content (even_tile_index + 1) with
      | Wall, Wall -> '~', st_wall_und
      | Wall, Empty | Empty, Wall -> ' ', st_wall_und
      | Wall, Kong_box | Kong_box, Wall -> assert false
      | Kong_box, Kong_box -> '~', st_kong_box_und
      | Kong_box, Empty | Empty, Kong_box -> ' ', st_kong_box_und
      | Empty, Empty -> ' ', LTerm_style.none
    in
    buffer.(char_index) <- x
  done;
  let row =
    match pos with
    | `Top -> 1
    | `Bottom -> (nb_tiles_in_game / 8) + 2
  in
  ignore begin
    List.fold_left
      (fun col (s, style) ->
         LTerm_draw.draw_string ctx row col ~style s;
         col + String.length s
      )
      4
      (strings_of_buffer buffer)
  end
  
let string_of_tile_index wall_content tile_index =
  match wall_content tile_index, wall_content (tile_index + 1) with
  | Wall, Wall -> "ll", st_wall
  | Wall, Empty | Empty, Wall -> " l", st_wall
  | Wall, Kong_box | Kong_box, Wall -> assert false
  | Kong_box, Kong_box -> "ll", st_kong_box
  | Kong_box, Empty | Empty, Kong_box -> " l", st_kong_box
  | Empty, Empty -> "  ", LTerm_style.none

let draw_vertical_row ctx row_index wall_content nb_tiles_in_game =
  let left_even_tile_index = (nb_tiles_in_game / 2) - 2 * (row_index - 1) in
  let right_even_tile_index = 3 * (nb_tiles_in_game / 4) + 2 * (row_index - 1) in
  let s, style = string_of_tile_index wall_content left_even_tile_index in
  LTerm_draw.draw_string ctx (row_index + 1) 2  ~style s;
  let s, style = string_of_tile_index wall_content right_even_tile_index in
  LTerm_draw.draw_string ctx (row_index + 1) (4 + nb_tiles_in_game / 8) ~style s 
  

let draw_wall_line ctx row_index wall_content nb_tiles_in_games =
  if row_index = 0 then
    draw_horizontal_row ctx `Top wall_content nb_tiles_in_games
  else if row_index = (nb_tiles_in_games / 8) + 1 then
    draw_horizontal_row ctx `Bottom wall_content nb_tiles_in_games
  else
    draw_vertical_row ctx row_index wall_content nb_tiles_in_games
    
let draw_side_wind ctx row col wind =
  LTerm_draw.draw_string ctx (row - 1) col " ";
  LTerm_draw.draw_string ctx row col wind;
  LTerm_draw.draw_string ctx (row + 1) col " "

let draw_tile ctx width height player tile_size tile =
  let v_padding = (height - tile_size) / 2 in
  let h_padding = (width - tile_size) / 2 in
  let v_offset = 2 in
  let h_offset = 5 in
  let row, col =
    match player with
    | 0 -> height - v_offset - tile_size, h_padding
    | 1 -> v_padding, width - h_offset - tile_size
    | 2 -> v_offset, h_padding
    | 3 -> v_padding, h_offset
    | _ -> assert false
  in
  Tile_repr.draw_tileset ctx row col tile_size [Some tile]



class river nb_tiles kind =
  let river_content = Array.init 4 (fun i -> empty_player_content i) in
  let nb_stacks_per_side = nb_tiles / 8 in
  let wall_start = ref 0 in
  let nb_tiles_in_kong_box = ref 0 in
  let last_tile = ref (nb_tiles - 1) in
  let die_1 = ref None in
  let die_2 = ref None in
  let tile = ref None in
  let width = nb_stacks_per_side + 8 in
  let height = nb_stacks_per_side + 4 in
  let tile_size = ref 7 in
  object
    inherit t kind

    method width = width

    method height = height

    method set_wall_start x = wall_start := x

    method set_nb_tiles_in_kong_box x = nb_tiles_in_kong_box := x

    method set_last_tile x = last_tile := x

    method set_die_1 x = die_1 := x

    method set_die_2 x = die_2 := x

    method set_tile (x: (int * Tileset.tile) option) = tile := x

    method set_seat_wind player wind =
      river_content.(player).seat_wind <- wind

    method! draw ctx _focused_widget =
      let river_rec = {row1 = 0; col1 = 0; row2 = height; col2 = width} in
      LTerm_draw.draw_frame ctx river_rec LTerm_draw.Heavy;
      let river_ctx = LTerm_draw.sub ctx river_rec in
      LTerm_draw.draw_string_aligned river_ctx 0 LTerm_geom.H_align_center (Printf.sprintf "  %s  " (string_of_wind river_content.(2).seat_wind));
      let wall_content = wall_content_at_index nb_tiles !nb_tiles_in_kong_box !wall_start !last_tile in
      for i = 0 to height - 3 do
        draw_wall_line ctx i wall_content nb_tiles
      done;
      let center_row = height / 2 in
      draw_side_wind river_ctx center_row 0 (string_of_wind river_content.(3).seat_wind);
      draw_side_wind river_ctx center_row (river_rec.col2 - 1) (string_of_wind river_content.(1).seat_wind);
      LTerm_draw.draw_string_aligned river_ctx (height - 1) LTerm_geom.H_align_center (Printf.sprintf "  %s  " (string_of_wind river_content.(0).seat_wind));
      let v_die_padding =
        let space_left = height - 10 in
        match space_left mod 3 with
        | 2 -> space_left / 3 + 1
        | _ -> space_left / 3
      in
      let h_die_padding =
        (width - 7) / 2
      in
      begin match !die_1 with
      | None -> ()
      | Some x ->
        Die.draw_die river_ctx v_die_padding h_die_padding x

      end;
      begin match !die_2 with
      | None -> ()
      | Some x ->
        Die.draw_die river_ctx (height - 5 - v_die_padding) h_die_padding x
      end;
      match !tile with
      | None -> ()
      | Some (player, tile) -> draw_tile river_ctx width height player !tile_size tile 
  end
