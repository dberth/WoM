(*Copyright (C) 2015 Denis Berthod*)

open Tileset

let event_handler wakener rack event =
  let open LTerm_event in
  let open LTerm_key in
  match event with
  | Resize _ -> rack # queue_draw; true
  | Key {code = Escape; _} -> Lwt.wakeup wakener (); true
  | Key {code = Tab; _} ->
    rack # set_reverse_mode (not (rack # reverse_mode));
    rack # queue_draw;
    true
  | _ -> false
  
  

let gui =
  let%lwt term = Lazy.force LTerm.stdout in
  let waiter, wakener = Lwt.wait () in
  
  let rack = new Rack.rack "rack" in
  
  rack # on_event (event_handler wakener rack);

  let hand = [Some c1; Some c2; Some c3; Some c4; Some c5; Some c6; Some c7; Some c8; Some c9] in

  let exposed = [[Some rd; Some gd; Some wd]; [ Some ew; Some sw; Some ww; Some nw]] in

  let discard = [Some d1; Some d2; Some d3; Some d4; Some d5; Some d6; Some d7; Some d8; Some d9; Some b1; Some b2; Some b3; Some b4; Some b5; Some b6; Some b7; Some b8; Some b9] in

  for i = 0 to 3 do
    rack # set_hand i hand;
    rack # set_exposed i exposed;
    rack # set_discard i discard
  done;

  LTerm_widget.run term ~save_state: true rack waiter
  
let () = Lwt_main.run gui
