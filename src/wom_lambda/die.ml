(*Copyright (C) 2015 Denis Berthod*)


let row_1 = function
  | 1 -> "     "
  | 2 | 3 -> " o   "
  | 4 | 5 | 6 -> " o o "
  | _ -> assert false

let row_2 = function
  | 1 -> "  o  "
  | 2 | 4 -> "     "
  | 3 | 5 -> "  o  "
  | 6  -> " o o "
  | _ -> assert false

let row_3 = function
  | 1 -> "     "
  | 2 | 3 -> "   o "
  | 4 | 5 | 6 -> " o o "
  | _ -> assert false

let draw_die ctx row col x =
  LTerm_draw.draw_string ctx row col " _____ ";
  LTerm_draw.draw_string ctx (row + 1) col "|     |";
  LTerm_draw.draw_string ctx (row + 2) col "|     |";
  LTerm_draw.draw_string ctx (row + 3) col "|     |";
  LTerm_draw.draw_string ctx (row + 4) col "'-----'";
  LTerm_draw.draw_string ctx (row + 1) (col + 1) (row_1 x);
  LTerm_draw.draw_string ctx (row + 2) (col + 1) (row_2 x);
  LTerm_draw.draw_string ctx (row + 3) (col + 1) (row_3 x)  
    
