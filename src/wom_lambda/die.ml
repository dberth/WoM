(*Copyright (C) 2015 Denis Berthod*)


let put ctx row col ?style s =
  LTerm_draw.draw_string ctx row col ?style s

let draw_row_1 ctx row col x =
  let put s= put ctx (row + 1) (col + 1) s in
  match x with 
  | 1 -> ()
  | 2 | 3 -> put " o   "
  | 4 | 5 | 6 -> put " o o "
  | _ -> assert false

let st_red = LTerm_style.({none with foreground = Some red})

let draw_row_2 ctx row col x =
  let put ?style s = put ?style ctx (row + 2) (col + 1) s in
  match x with
  | 1 -> put ~style: st_red "  @  "
  | 2 | 4 -> ()
  | 3 | 5 -> put "  o  "
  | 6  -> put " o o "
  | _ -> assert false

let draw_row_3 ctx row col x =
  let put ?style s = put ctx (row + 2) (col + 1) s in
  match x with
  | 1 -> ()
  | 2 | 3 -> put "   o "
  | 4 | 5 | 6 -> put " o o "
  | _ -> assert false

let draw_die ctx row col x =
  LTerm_draw.draw_string ctx row col ".-----.";
  LTerm_draw.draw_string ctx (row + 1) col "|     |";
  LTerm_draw.draw_string ctx (row + 2) col "|     |";
  LTerm_draw.draw_string ctx (row + 3) col "|     |";
  LTerm_draw.draw_string ctx (row + 4) col "'-----'";
  draw_row_1 ctx row col x;
  draw_row_2 ctx row col x;
  draw_row_3 ctx row col x  
    
