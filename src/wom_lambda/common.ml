(*Copyright (C) 2014 Denis Berthod*)

type wind =
  | East
  | South
  | West
  | North

let string_of_wind = function
  | East -> "E"
  | South -> "S"
  | West -> "W"
  | North -> "N"

