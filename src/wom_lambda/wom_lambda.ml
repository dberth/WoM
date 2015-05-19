(*Copyright (C) 2015 Denis Berthod*)

open Tileset

let event_handler wakener rack event =
  let open LTerm_event in
  let open LTerm_key in
  match event with
  | Resize _ -> rack # queue_draw; true
  | Key {code = Escape; _} -> Lwt.wakeup wakener (); true
  | _ -> false
  
  

let gui =
  let%lwt term = Lazy.force LTerm.stdout in
  let waiter, wakener = Lwt.wait () in
  
  let rack = new Rack.rack "rack" in
  
  rack # on_event (event_handler wakener rack);

  rack # set_hand 1 [Some c1; Some d1; Some b1; Some rd; Some wd; Some gd; Some ew; Some ww; Some nw; Some sw];

  rack # set_exposed 1 [[Some b3; Some b4; Some b5];[Some c7; Some c7; Some c7];[None; Some d6; Some d6; None]];

  rack # set_discard 1 [Some wd; Some rd; Some d1; Some c1; Some b1; Some gd; Some ew; Some sw; Some ww; Some nw];

  (* rack # set_hand 0 [Some d1; Some d2; Some d3; Some d4; Some d5; Some d6; Some d7; Some d8; Some d9]; *)

  (*rack # set_exposed 0 [[Some b1; Some b2; Some b3; Some b4; Some b5; Some b6; Some b7; Some b8; Some b9]];*)

  rack # set_hand 0 [Some c1; Some c2; Some c3; Some c4; Some c5; Some c6; Some c7; Some c8; Some c9];

  rack # set_exposed 0 [[Some rd; Some gd; Some wd]; [ Some ew; Some sw; Some ww; Some nw]];

  LTerm_widget.run term ~save_state: true rack waiter
  
let () = Lwt_main.run gui
