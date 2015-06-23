(*Copyright (C) 2015 Denis Berthod*)

class console: string ->
  object
    inherit LTerm_widget.t

    method writeln: string -> unit
  end
