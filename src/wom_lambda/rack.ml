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
    mutable discard: tile option list
  }

let draw_tileset ctx row col tile_size tiles =
  let open LTerm_draw in
  let nb = List.length tiles in
  if nb <> 0 then begin
    let draw_row offset sep inner =
      draw_string ctx (row + offset) col sep;
      for i = 0 to nb - 1 do
        draw_string ctx (row + offset) (col + i * (tile_size - 1) + 1) (String.make (tile_size - 2) inner ^ sep)
      done
    in
    draw_row 0 " " '_';
    for i = 1 to tile_size - 2 do
      draw_row i "|" ' '
    done;
    draw_row (tile_size - 1) "'" '-';
    List.iteri
      (fun i tile ->
         match tile with
         | None -> ()
         | Some tile ->
           let tile_descr = tile_descr_of_tile tile in
           Tile_repr.draw_tile_content ctx (row + 1) (col + i * (tile_size -1) + 1) tile_size tile_descr
      )
      tiles
  end

let tileset_size tileset tile_size =
  let nb = List.length tileset in
  (tile_size - 1) * nb + 1

let draw_tilesets ctx row col tile_size tilesets =
  ignore begin
    List.fold_left
      (fun left tiles ->
         draw_tileset ctx row left tile_size tiles;
         left + tileset_size tiles tile_size
      )
      col
      tilesets
  end

let draw_tile ctx row col tile_size tile_descr = draw_tileset ctx row col tile_size [tile_descr]


let exposed_size exposed tile_size =
  List.fold_left
    (fun acc tileset -> acc + tileset_size tileset tile_size)
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
    ctx
    {hand; exposed; discard = discard_tiles}
  =
  let open LTerm_draw in
  let open LTerm_geom in
  let inner_height =
    if separator then
      height + discard + 1
    else
      height + discard
  in
  let draw_hand_and_exposed border  =
    draw_tileset ctx (top + 1) (left + border) height hand;
    match exposed with
    | [] -> ()
    | _ ->
      let exposed_size = exposed_size exposed height in
      draw_tilesets ctx (top + 1) (left + width - 1 - exposed_size) height exposed
  in
  begin
    if border then begin
      let row2 = top + inner_height + 2 in
      draw_frame
        ctx
        {row1 = top; row2; col1 = left; col2 = left + width}
        Heavy;
      if separator then draw_hline ctx (top + height + 1) (left + 1) (width - 2) Light;
      if discard <> 0 then begin
        let offset = if separator then 2 else 1 in
        draw_tileset ctx (top + height + offset) (left + 1) discard discard_tiles
      end;
      draw_hand_and_exposed 1;
      draw_string ctx 0 0 (Printf.sprintf "height: %i, inner_height: %i\n%!" height inner_height);
      row2
    end else begin
      draw_hline ctx top left width Heavy;
      draw_hand_and_exposed 0;
      if discard <> 0 then draw_tileset ctx (top + height + 1) left discard discard_tiles;
      top + inner_height + 1
    end
  end

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
  

let empty_player_rack () =
  {
    hand = [];
    exposed = [];
    discard = [];
  }

class rack kind =
  let rack_content = Array.init 4 (fun _ -> empty_player_rack ())in  
  object
    inherit t kind

    method set_hand player hand = rack_content.(player).hand <- hand

    method set_discard player discard = rack_content.(player).discard <- discard

    method set_exposed player exposed = rack_content.(player).exposed <- exposed 
      
    method! draw ctx _focused_widget =
      let open LTerm_geom in
      let {cols; rows} as size = LTerm_draw.size ctx in
      match config_of_size size with
      | None -> () (*TODO put a message*)
      | Some 
          {
            padding_left;
            padding_top;
            width;
            main_size;
            other_size;
            discard_size;
            border;
            separator
          } ->
        let draw_rack ~top ~height  player =
          draw_rack ~top ~left: padding_left ~separator ~border ~width ~height ~discard: discard_size ctx rack_content.(player)
        in
        let top = draw_rack ~top: padding_top ~height: other_size 1 in
        let top = draw_rack ~top ~height: other_size 2 in
        let top = draw_rack ~top ~height: other_size 3 in
        ignore (draw_rack ~top ~height: main_size 0)
      
  end