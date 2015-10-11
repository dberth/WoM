(*Copyright (C) 2015 Denis Berthod*)

open LTerm_widget
open Tileset

type rack_config =
  {
    padding_left: int;
    padding_top: int;
    width: int;
    main_size: int;
    other_size: int;
    discard_size: int;
    border: bool;
    separator: bool;
  }

type player_rack_content =
  {
    mutable hand: tile option list;
    mutable exposed: tile option list list;
    mutable discard: tile option list;
    mutable name: string option;
    mutable seat_wind: Common.wind;
  }

let exposed_size exposed tile_size =
  List.fold_left
    (fun acc tileset -> acc + Tile_repr.tileset_size tileset tile_size)
    0
    exposed

let draw_rack
    ~top
    ~left
    ~separator
    ~border
    ~width
    ~height
    ~discard
    ~reverse
    ~selected_tileset
    ctx
    {hand; exposed; discard = discard_tiles; name; seat_wind}
  =
  let open LTerm_draw in
  let open LTerm_geom in
  let inner_height =
    if separator then
      height + discard + 1
    else
      height + discard
  in
  let draw_hand_and_exposed draw_discard row border height =
    if draw_discard then
      Tile_repr.draw_tileset ctx row (left + border) height discard_tiles
    else begin
      Tile_repr.draw_tileset ?selected_tileset ctx row (left + border) height hand;
      match exposed with
      | [] -> ()
      | _ ->
        let exposed_size = exposed_size exposed height in
        Tile_repr.draw_tilesets ctx row (left + width - 1 - exposed_size) height exposed
    end
  in
  let new_top =
    if border then begin
      let row2 = top + inner_height + 2 in
      draw_frame
        ctx
        {row1 = top; row2; col1 = left; col2 = left + width}
        Heavy;
      if separator then draw_hline ctx (top + height + 1) (left + 1) (width - 2) Light;
      if discard <> 0 then begin
        let offset = if separator then 2 else 1 in
        draw_hand_and_exposed (not reverse) (top + height + offset) 1 discard
      end;
      draw_hand_and_exposed reverse (top + 1) 1 height;
      row2
    end else begin
      draw_hline ctx top left width Heavy;
      draw_hand_and_exposed reverse (top + 1) 0 height;
      if discard <> 0 then
        draw_hand_and_exposed (not reverse) (top + height + 1) 0 discard;
      top + inner_height + 1
    end
  in
  let seat_wind = Printf.sprintf " %s " (Common.string_of_wind seat_wind) in
  begin
    match name with
    | None -> ()
    | Some name ->
      let max_size = width - 8 - String.length seat_wind in
      let name =
        if max_size < String.length name then
          String.sub name 0 max_size
        else
          name
      in
      LTerm_draw.draw_string ctx top (left + 1) (Printf.sprintf "< %s >" name)
  end;
  LTerm_draw.draw_string ctx top (left + width - 3 - String.length seat_wind) (Printf.sprintf "<%s>" seat_wind);
  new_top


let mk_config ~main_size ?other_size ?(discard_size = 0) ?(border = false) ?(separator = false) ~width rows cols =
  let other_size =
    match other_size with
    | None -> main_size
    | Some size -> size
  in
  let height = 3 * other_size + main_size + 4 * discard_size in
  let height = if border then height + 8 else height + 4 in
  let height = if separator then height + 4 else height in
  let width =
    if border then
      width + 2
    else
      width
  in
  if cols < width || rows < height then
    None
  else
    Some
      {
        padding_left = (cols - width) / 2;
        padding_top = (rows - height) / 2;
        width;
        main_size;
        other_size;
        discard_size;
        border;
        separator;
      }

let (&&&) opt f =
  match opt with
  | None -> f ()
  | Some x -> Some x

let small_tile_config cols rows =
  let try_config ?discard_size ?border ?separator () =
    mk_config ~main_size: 3 ~other_size: 3 ?discard_size ?border ?separator ~width: 42 rows cols
  in
  try_config ~discard_size: 3 ~border: true ~separator: true () &&&
  try_config ~discard_size: 3 ~border: true &&&
  try_config ~discard_size: 3 &&&
  try_config ~border: true &&&
  try_config
  
let medium_tile_config cols rows =
  let try_config ?other_size ?discard_size ?border ?separator () =
    mk_config ~main_size: 5 ?other_size ?discard_size ?border ?separator ~width: 78 rows cols
  in
  try_config ~discard_size: 5 ~border: true ~separator: true () &&&
  try_config ~discard_size: 5 ~border: true &&&
  try_config ~discard_size: 5 &&&
  try_config ~discard_size: 3 ~border: true ~separator: true &&&
  try_config ~discard_size: 3 ~border: true &&&
  try_config ~discard_size: 3 &&&
  try_config ~border: true &&&
  try_config  &&&
  try_config ~other_size: 3 ~border: true &&&
  try_config ~other_size: 3

let large_tile_config cols rows =
  let try_config ?other_size ?discard_size ?border ?separator () =
    mk_config ~main_size: 7 ?other_size ?discard_size ?border ?separator ~width: 114 rows cols
  in
  try_config ~discard_size: 7 ~border: true ~separator: true () &&&
  try_config ~discard_size: 7 ~border: true &&&
  try_config ~discard_size: 7 &&&
  try_config ~discard_size: 5 ~border: true ~separator: true &&&
  try_config ~discard_size: 5 ~border: true &&&
  try_config ~discard_size: 5 &&&
  try_config ~other_size: 5 ~discard_size: 5 ~border: true &&&
  try_config ~other_size: 5 ~discard_size: 5 &&&
  try_config ~other_size: 5 ~discard_size: 3 ~border: true ~separator: true &&&
  try_config ~other_size: 5 ~discard_size: 3 ~border: true &&&
  try_config ~other_size: 5 ~discard_size: 3 &&&
  try_config ~other_size: 3 ~discard_size: 3 ~border: true &&&
  try_config ~other_size: 3 ~discard_size: 3 &&&
  try_config ~other_size: 5 ~border: true &&&
  try_config ~other_size: 5


let config_of_size {LTerm_geom.cols; rows} =
  let try_config limit config_fun =
    if limit <= cols then
      config_fun cols rows
    else
      None
  in
  match try_config 114 large_tile_config with
  | Some config -> Some config
  | None ->
    match try_config 78 medium_tile_config with
    | Some config -> Some config
    |  None -> try_config 42 small_tile_config
  

let empty_player_rack player =
  let open Common in
  let seat_wind =
    match player with
    | 0 -> East
    | 1 -> South
    | 2 -> West
    | 3 -> North
    | _ -> assert false
  in
  {
    hand = [];
    exposed = [];
    discard = [];
    name = None;
    seat_wind;
  }

type player = int

let cs_empty = [], None, []

let cs_of_list = function
  | [] -> cs_empty
  | hd :: tl -> [], Some hd, tl

let cs_value (_, value, _) = value

let rec cs_next cs =
  match cs with
  | _, None, _ | [], _, [] -> cs
  | prev, Some value, hd :: tl -> value :: prev, Some hd, tl
  | prev, Some value, [] -> cs_next ([], Some value, List.rev prev)

let rec cs_prev cs =
  match cs with
  | _, None, _ | [], _, [] -> cs
  | hd :: tl, Some value, next -> tl, Some hd, value :: next
  | [], Some value, next -> cs_prev (List.rev next, Some value, [])

let cs_length (prev, value, next) =
  match value with
  | None -> 0
  | Some _ -> List.length prev + 1 + List.length next

let cs_select cs elem =
  let rec aux limit cs =
    match cs_value cs with
    | None -> cs
    | Some value when value = elem -> cs
    | _ ->
      if limit < 0 then
        raise Not_found
      else
        aux (limit - 1) (cs_next cs)
  in
  aux (cs_length cs) cs

let compare_tiles_opt to1 to2 =
  match to1, to2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some tile1, Some tile2 -> Tileset.compare_tiles tile1 tile2 

let rec compare_tilesets ts1 ts2 =
  match ts1, ts2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | hd1 :: tl1, hd2 :: tl2 ->
    let x = compare_tiles_opt hd1 hd2 in
    if x = 0 then compare_tilesets tl1 tl2 else x

class rack kind =
  let rack_content = Array.init 4 (fun i -> empty_player_rack i) in
  let config ctx =
      let open LTerm_geom in
      let {cols; rows} as size = LTerm_draw.size ctx in
      config_of_size size
  in
  let selected_tileset = ref cs_empty in
  object
    inherit t kind

    val mutable reverse = false

    method set_hand player hand = rack_content.(player).hand <- (List.sort compare_tiles_opt hand)

    method set_discard player discard = rack_content.(player).discard <- discard

    method set_exposed player exposed = rack_content.(player).exposed <- exposed

    method set_name player name = rack_content.(player).name <- Some name

    method set_seat_wind player wind = rack_content.(player).seat_wind <- wind

    method set_reverse_mode mode = reverse <- mode

    method reverse_mode = reverse

    method clear_selection = selected_tileset := cs_empty

    method select_next_tileset = selected_tileset := cs_next !selected_tileset 

    method select_prev_tileset = selected_tileset := cs_prev !selected_tileset
      
    method selected_tileset = cs_value !selected_tileset

    method set_tilesets tilesets =
      let tilesets = List.map (List.sort compare_tiles_opt) tilesets in
      let tilesets = List.sort compare_tilesets tilesets in
      (* print_endline "++++>"; *)
      (* List.iter *)
      (*   (fun tileset -> *)
      (*      print_endline "----"; *)
      (*      List.iter *)
      (*        (function None -> print_endline "None" *)
      (*          | Some tile -> print_endline (Game_descr.string_of_tile tile) *)
      (*        ) *)
      (*        tileset; *)
      (*      print_endline "----"; *)
      (*   ) *)
      (*   tilesets; *)
      (* print_endline "<++++"; *)
      selected_tileset := cs_of_list tilesets

    method set_selected_tileset tileset = selected_tileset := cs_select !selected_tileset tileset

    method width ctx =
      match config ctx with
      | None -> None
      | Some {width; _} -> Some width
      

    method! draw ctx _focused_widget =
      match config ctx with
      | None -> ()
      | Some
          {
            padding_left;
            padding_top;
            width;
            main_size;
            other_size;
            discard_size;
            border;
            separator;
          } ->
        let draw_rack ?selected_tileset ~top ~height  player =
          draw_rack ~top ~left: padding_left ~separator ~border ~width ~height ~discard: discard_size ~reverse ~selected_tileset ctx rack_content.(player)
        in
        let top = draw_rack ~top: padding_top ~height: other_size 1 in
        let top = draw_rack ~top ~height: other_size 2 in
        let top = draw_rack ~top ~height: other_size 3 in
        ignore (draw_rack ?selected_tileset: (cs_value !selected_tileset) ~top ~height: main_size 0)
      
  end
