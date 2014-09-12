(*Copyright (C) 2014 Denis Berthod*)

type tile

type basic_tileset (*all the tiles are of the same kind: Dot, Bam, Char, each kind of dragons or each kind of winds*)

type tileset

type irregular_hands

type mahjong =
  | Regular of basic_tileset list
  | Irregular of tileset

type tile_descr =
  | Bam of int
  | Dot of int
  | Char of int
  | Red_dragon
  | Green_dragon
  | White_dragon
  | East_wind
  | South_wind
  | West_wind
  | North_wind

val empty: tileset

val tileset_of_basic_tileset: basic_tileset -> tileset

val add_tile: tile -> tileset -> tileset

val remove_tile: tile -> tileset -> tileset

val compare_tiles: tile -> tile -> int

val c1: tile
val c2: tile
val c3: tile
val c4: tile
val c5: tile
val c6: tile
val c7: tile
val c8: tile
val c9: tile
val d1: tile
val d2: tile
val d3: tile
val d4: tile
val d5: tile
val d6: tile
val d7: tile
val d8: tile
val d9: tile
val b1: tile
val b2: tile
val b3: tile
val b4: tile
val b5: tile
val b6: tile
val b7: tile
val b8: tile
val b9: tile
val rd: tile
val wd: tile
val gd: tile
val ew: tile
val sw: tile
val ww: tile
val nw: tile

val tile_pred_pred: tile -> tile option
val tile_pred: tile -> tile option
val tile_succ: tile -> tile option
val tile_succ_succ: tile -> tile option

val tileset_of_tiles: tile list -> tileset

val no_irregular_hand: irregular_hands

val add_irregular_hand: tile list -> irregular_hands -> irregular_hands

val mahjong: ?irregular_hands: irregular_hands -> int -> tileset -> mahjong list

val is_pair: tileset -> bool

val is_chow: tileset -> bool

val is_kong: tileset -> bool

val is_pong: tileset -> bool

val tile_descr_of_basic_tileset: basic_tileset -> tile_descr list

val tile_descr_of_tile: tile -> tile_descr

val tile_descr_of_tileset: tileset -> tile_descr list

val tile_descr_of_mahjong: mahjong -> tile_descr list list

val tiles_of_tileset: tileset -> tile list

val string_of_tile_descr: tile_descr -> string

val string_of_tile: tile -> string

val tiles_to_chow: tile_descr -> tile_descr list list

type tileset_status =
  {
    alone: tile_descr list;
    in_sub_chow: tile_descr list;
    in_pair: tile_descr list;
    in_3set: tile_descr list;
  }

val status_of_tileset: tileset -> tileset_status

module Map: sig
  type 'a t

  val empty: 'a t

  val add: tile -> 'a -> 'a t -> 'a t

  val remove: tile -> 'a t -> 'a t

  val find_default: tile -> 'a -> 'a t -> 'a

  val update: ('a option -> 'a option) -> tile -> 'a t -> 'a t

  val fold: (tile -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter: (tile -> 'a -> unit) -> 'a t -> unit
end

module Set: sig
  type t

  val empty: t

  val add: tile -> t -> t

  val remove: tile -> t -> t

  val cardinal: t -> int

  val iter: (tile -> unit) -> t -> unit

  val elements: t -> tile list
end
