(*Copyright (C) 2015 Denis Berthod*)

open LTerm_widget
open LTerm_geom

let rec draw_lines ctx lines last_line =
  if 0 <= last_line then
    match lines with
    | [] -> ()
    | hd :: tl ->
      LTerm_draw.draw_string ctx last_line 0 hd;
      draw_lines ctx tl (last_line - 1)

class console kind =
  let lines = ref [] in
  object
    inherit t kind

    method writeln s = lines := s :: !lines

    method! draw ctx _focused_widget =
      let {rows; _} = LTerm_draw.size ctx in 
      let last_line = min (rows - 1) (List.length !lines) in
      draw_lines ctx !lines last_line

  end
