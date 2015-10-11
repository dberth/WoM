(*Copyright (C) 2015 Denis Berthod*)

type tile_size = int

val tileset_size: Tileset.tile option list -> tile_size -> int

val draw_tileset:
  ?selected_tileset: Tileset.tile option list ->
  LTerm_draw.context ->
  int (*row*) ->
  int (*col*) ->
  tile_size ->
  Tileset.tile option list -> unit

val draw_tilesets:
  LTerm_draw.context ->
  int (*row*) ->
  int (*col*) ->
  tile_size ->
  Tileset.tile option list list -> unit
