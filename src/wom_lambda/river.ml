(*Copyright (C) 2015 Denis Berthod*)

open LTerm_widget
open LTerm_geom


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
    

class river nb_tiles kind =
  let river_content = Array.init 4 (fun i -> empty_player_content i) in
  let nb_stacks_per_side = nb_tiles / 8 in
  let wall_start = ref 41 in
  let nb_tiles_in_kong_box = ref 13 in
  let last_tile = ref (*(nb_tiles - 1)*) 120 in
  object
    inherit t kind

    method width = nb_stacks_per_side + 8

    method! draw ctx _focused_widget =
      let river_rec = {row1 = 0; col1 = 0; row2 = nb_stacks_per_side + 4; col2 = nb_stacks_per_side + 8} in
      LTerm_draw.draw_frame ctx river_rec LTerm_draw.Heavy;
      let river_ctx = LTerm_draw.sub ctx river_rec in
      LTerm_draw.draw_string_aligned river_ctx 0 LTerm_geom.H_align_center (Printf.sprintf "  %s  " river_content.(2).seat_wind);
      let wall_content = wall_content_at_index nb_tiles !nb_tiles_in_kong_box !wall_start !last_tile in
      for i = 0 to (nb_tiles / 8) + 1 do
        draw_wall_line ctx i wall_content nb_tiles
      done;
      (* LTerm_draw.draw_string ~style: st_wall_und ctx 1 4 "~~~~~~~~~~~~~~~~~"; *)
      (* LTerm_draw.draw_string ctx 2 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 3 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 4 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 5 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 6 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 7 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 8 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 9 0 "  ll                 ll  "; *)
      (* LTerm_draw.draw_string ctx 10 0 *)
      (*   (Printf.sprintf "%s ll                 ll %s" river_content.(3).seat_wind river_content.(1).seat_wind); *)
      (* LTerm_draw.draw_string ctx 11 0 "  ll                 ll  "; *)
      (* LTerm_draw.draw_string ctx 12 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 13 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 14 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 15 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 16 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 17 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ctx 18 2 "ll                 ll"; *)
      (* LTerm_draw.draw_string ~style: st_wall_und ctx 19 4 "~~~~~~~~~~~~~~~~~~"; *)
      LTerm_draw.draw_string_aligned river_ctx 20 LTerm_geom.H_align_center (Printf.sprintf "  %s  " river_content.(0).seat_wind)


  end
