(*Copyright (C) 2015 Denis Berthod*)

open Tileset
open LTerm_draw
open LTerm_style

let st_green = {none with foreground = Some lgreen}
let st_red = {none with foreground = Some lred}
let st_bold = {none with bold = Some true}
let st_blue = {none with foreground = Some lblue}
let st_underline = {none with underline = Some true}
let st_red_und = {st_red with underline = Some true}
let st_blue_und = {st_blue with underline = Some true}
let st_green_und = {st_green with underline = Some true}

let draw_tile_3 put tile_descr =
  let put = put 0 in
  match tile_descr with
  | Bam x -> put st_green (string_of_int x) 
  | Dot x -> put st_bold (string_of_int x)
  | Char x -> put st_red (string_of_int x)
  | Red_dragon -> put st_red "D"
  | Green_dragon -> put st_green "D"
  | White_dragon -> put none "D"
  | East_wind -> put st_blue "E"
  | South_wind -> put st_blue "S"
  | West_wind -> put st_blue "W"
  | North_wind -> put st_blue "N"

let draw_tile_5_1 put tile_descr =
  match tile_descr with
  | Bam x
  | Dot x -> put 0 none (string_of_int x)
  | Char  x ->
    put 0 none (string_of_int x);
    put 1 st_underline "-"
  | Red_dragon -> put 1 st_red_und ";"
  | Green_dragon -> put 1 st_green ","
  | White_dragon -> put 0 none "###"
  | East_wind -> put 0 none "E"; put 1 st_blue "."
  | South_wind -> put 0 none "S"
  | West_wind -> put 0 none "W"
  | North_wind -> put 0 none "N"

let draw_tile_5_2 put tile_descr =
  match tile_descr with
  | Bam _ -> put 1 st_green "8"
  | Dot _ -> put 1 st_bold "o"
  | Char _ -> put 1 st_red "@"
  | Red_dragon ->
    put 0 st_red "[";
    put 1 st_red_und "|";
    put 2 st_red "]"
  | Green_dragon -> put 0 st_green "/^\\"
  | White_dragon -> put 0 none "# #"
  | East_wind -> put 0 st_blue "[|]"
  | South_wind -> put 1 st_blue_und "+"
  | West_wind -> put 0 st_blue "---"
  | North_wind -> put 0 st_blue ",L"

let draw_tile_5_3 put tile_descr =
  match tile_descr with
  | Bam _ -> put 1 st_green "8"
  | Dot _ -> put 1 st_bold "o"
  | Char _ -> put 0 st_red "^^^"
  | Red_dragon -> put 1 st_red "|"
  | Green_dragon -> put 0 st_green "5!J"
  | White_dragon -> put 0 none "###"
  | East_wind -> put 0 st_blue "'j`"
  | South_wind -> put 0 st_blue "i+]"
  | West_wind -> put 0 st_blue "\\H/"
  | North_wind -> put 0 st_blue "/L"

let draw_tile_7_1 put tile_descr =
  match tile_descr with
  | Dot 7 -> put 1 st_bold "@"
  | Bam 1 | Dot 1 -> put 0 none "1"
  | Char 1 -> put 0 none "1 __"
  | Char 2 -> put 0 none "2"; put 2 none "_"
  | Char 3 -> put 0 none "3-"; put 2 st_underline "-"; put 3 none "-"
  | Char 4 -> put 0 none "4 __"
  | Char 6 -> put 0 none "6"; put 1 st_underline " l "
  | Char 7 -> put 0 none "7"; put 1 st_underline " l " 
  | Char x -> put 0 none (string_of_int x)
  | Dot _
  | Bam _
  | Red_dragon
  | Green_dragon
  | White_dragon -> ()
  | East_wind -> put 0 none "E"
  | South_wind -> put 0 none "S"
  | West_wind -> put 0 none "W"
  | North_wind -> put 0 none "N"

let draw_tile_7_2 put tile_descr =
  match tile_descr with
  | Dot 1 -> put 2 st_red "_"
  | Dot (2 | 7) -> put 2 st_bold "@"
  | Dot 3  ->  put 1 st_bold "@"
  | Dot (4 | 5 | 6 | 8) -> put 1 st_bold "@ @"
  | Dot 9 -> put 0 st_bold "@ @ @"
  | Bam 1 -> put 1 st_green ">')"
  | Bam 2 -> put 2 st_green "8"
  | Bam (3 | 7) -> put 2 st_red "8" 
  | Bam (4 | 5)  -> put 1 st_green "8 8"
  | Bam 8 -> put 1 st_green "8/\\8"
  | Bam 9 -> put 0 st_green "8"; put 2 st_red "8"; put 4 st_green "8"
  | Char (2 | 3) -> put 1 none "---"
  | Char 4 -> put 1 none "["; put 2 st_underline "()"; put 4 none "]"
  | Char 5 -> put 1 none "1"; put 2 st_underline "In"
  | Char (6 | 8) -> put 1 none "/ \\"
  | Char 7 -> put 2 none "|_"
  | Char 9 -> put 1 none "/-"; put 3 st_underline "i"
  | _ -> ()

let draw_tile_7_3 put tile_descr =
  match tile_descr with
  | Dot 1 -> put 1 st_red "("; put 2 st_bold "@"; put 3 st_red ")"
  | Dot (3 | 5) -> put 2 st_red "@"
  | Dot 7 -> put 3 st_bold "@"
  | Dot 8 -> put 1 st_bold "@ @"
  | Dot 9 -> put 0 st_red "@ @ @"
  | Bam 1 -> put 1 st_green "( \\"
  | Bam 5 -> put 2 st_red "8"
  | Bam (6 | 7) -> put 0 st_green "8 8 8"
  | Bam 9 -> put 0 st_green "8"; put 2 st_red "8"; put 4 st_green "8"
  | Char _ -> put 1 st_red "+;-"
  | _ -> ()

let draw_tile_7_4 put tile_descr =
  match tile_descr with
  | Dot 2 -> put 2 st_bold "@"
  | Dot 3 -> put 3 st_bold "@"
  | Dot (4 | 5 | 8) -> put 1 st_bold "@ @"
  | Dot (6 | 7) -> put 1 st_red "@ @"
  | Dot 9 -> put 0 st_bold "@ @ @"
  | Bam 1 -> put 0 st_green_und "--\"-\\"
  | Bam 2 -> put 2 st_green "8"
  | Bam (3 | 4 | 5) -> put 1 st_green "8 8"
  | Bam (6 | 7) -> put 0 st_green "8 8 8"
  | Bam 8 -> put 1 st_green "8\\/8"
  | Bam 9 -> put 0 st_green "8"; put 2 st_red "8"; put 4 st_green "8"
  | Char _ -> put 0 st_red "_[+]"
  | _ -> ()

let draw_tile_7_5 put tile_descr =
  match tile_descr with
  | Dot (6 | 7) -> put 1 st_red "@ @"
  | Dot 8 -> put 1 st_bold "@ @"
  | Char _ -> put 0 st_red "|'-+)"
  | _ -> ()

let draw_tile_content ctx row col size tile_descr =
  let put row_offset col_offset style s =
    draw_string ctx (row + row_offset) (col + col_offset) ~style s
  in
  match size with
  | 3 -> draw_tile_3 (put 0) tile_descr
  | 5 ->
    draw_tile_5_1 (put 0) tile_descr;
    draw_tile_5_2 (put 1) tile_descr;
    draw_tile_5_3 (put 2) tile_descr
  | 7 ->
    draw_tile_7_1 (put 0) tile_descr;
    draw_tile_7_2 (put 1) tile_descr;
    draw_tile_7_3 (put 2) tile_descr;
    draw_tile_7_4 (put 3) tile_descr;
    draw_tile_7_5 (put 4) tile_descr
  | _ -> assert false     
  
