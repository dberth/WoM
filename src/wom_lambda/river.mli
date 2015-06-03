(*Copyright (C) 2015 Denis Berthod*)

class river: int -> string ->
  object
    inherit LTerm_widget.t

    method width: int
  end
