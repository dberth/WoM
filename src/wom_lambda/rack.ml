(*Copyright (C) 2015 Denis Berthod*)

open LTerm_widget

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


let draw_tileset ctx row col tile_size nb =
  let open LTerm_draw in
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
  draw_row (tile_size - 1) "'" '-'

let draw_tile ctx row col tile_size = draw_tileset ctx row col tile_size 1
  
let draw_full_rack ctx row col tile_size =
  let tileset nb col = draw_tileset ctx row col tile_size nb; col + (tile_size - 1 )* nb + 1 in
  tileset 2 col |>
  tileset 4 |>
  tileset 4 |>
  tileset 4 |>
  tileset 4 |> fun _ -> ()

let draw_rack ~top ~left ~separator ~border ~width ~height ~discard ctx =
  let open LTerm_draw in
  let open LTerm_geom in
  let inner_height =
    if separator then
      height + discard + 1
    else
      height + discard
  in
  begin
    if border then begin
      let row2 = top + inner_height + 2 in
      draw_frame
        ctx
        {row1 = top; row2; col1 = left; col2 = left + width}
        Heavy;
      if separator then draw_hline ctx (top + height + 1) (left + 1) (width - 2) Light;
      draw_full_rack ctx (top + 1) (left + 1) height;
      if discard <> 0 then begin
        let offset = if separator then 2 else 1 in
        draw_tile ctx (top + height + offset) (left + 1) discard
      end;
      draw_string ctx 0 0 (Printf.sprintf "height: %i, inner_height: %i\n%!" height inner_height);
      row2
    end else begin
      draw_hline ctx top left width Heavy;
      draw_full_rack ctx (top + 1) left height;
      if discard <> 0 then draw_tile ctx (top + height + 1) left discard;
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
  

class rack kind =
  object
    inherit t kind
      
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
        let draw_rack ~top ~height =
          draw_rack ~top ~left: padding_left ~separator ~border ~width ~height ~discard: discard_size ctx
        in
        let top = draw_rack ~top: padding_top ~height: other_size in
        let top = draw_rack ~top ~height: other_size in
        let top = draw_rack ~top ~height: other_size in
        ignore (draw_rack ~top ~height: main_size)
      
  end
