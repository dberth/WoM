(*Copyright (C) 2015 Denis Berthod*)

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

  LTerm_widget.run term ~save_state: true rack waiter
  
let () = Lwt_main.run gui
