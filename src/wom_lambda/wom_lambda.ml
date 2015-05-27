(*Copyright (C) 2015 Denis Berthod*)

open Tileset
open LTerm_widget

let event_handler wakener rack playground event =
  let open LTerm_event in
  let open LTerm_key in
  match event with
  | Resize _ -> rack # queue_draw; true
  | Key {code = Escape; _} -> Lwt.wakeup wakener (); true
  | Key {code = Tab; _} ->
    rack # set_reverse_mode (not (rack # reverse_mode));
    playground # queue_draw;
    true
  | _ -> false

let calculate_padding ctx ~rack_width ~side_width =
  let open LTerm_geom in
  let {cols; _} = LTerm_draw.size ctx in
  let remaining_space = cols - rack_width - side_width in
  let base = remaining_space / 3 in
  match remaining_space mod 3 with
  | 0 -> base, base, base
  | 1 -> base, base + 1, base
  | 2 -> base + 1, base, base + 1
  | _ -> assert false
  
class playground (rack: Rack.rack) (river: River.river) =
  object (this)
    inherit t "playground"

    method! draw ctx focused_widget =
      let open LTerm_geom in
      let {rows; cols} = LTerm_draw.size ctx in
      let side_width = 22 in

      let rack_rec = {row1 = 0; col1 = 0; row2 = rows; col2 = cols - side_width} in
      let rack_ctx = LTerm_draw.sub ctx rack_rec in
      match rack # width rack_ctx with
      | None -> LTerm_draw.draw_string ctx 0 0 "Screen is too small."
      | Some rack_width ->
        let left_padding, _middle_padding, right_padding = calculate_padding ctx ~rack_width ~side_width in
        let rack_rec = {rack_rec with col1 = left_padding; col2 = left_padding + rack_width } in
      let side_rec = {row1 = 0; col1 = cols - side_width - right_padding; row2 = rows ; col2 = cols} in
        (* LTerm_draw.draw_frame ctx rack_rec LTerm_draw.Light; *)
        (* LTerm_draw.draw_frame ctx side_rec LTerm_draw.Light; *)
        rack # draw (LTerm_draw.sub ctx rack_rec) focused_widget;
        river # draw (LTerm_draw.sub ctx side_rec) focused_widget;
        (* for i = 0 to 150 do *)
        (*   LTerm_draw.draw_string ctx 0 i (string_of_int (i mod 10)) *)
        (* done; *)
        (* LTerm_draw.draw_string ctx 1 0 (Printf.sprintf "%i, %i, %i" left_padding middle_padding right_padding) *)

    initializer
      rack # set_parent (Some (this :> t));
      river # set_parent (Some (this :> t))
  end

let gui =
  let%lwt term = Lazy.force LTerm.stdout in
  let waiter, wakener = Lwt.wait () in
  
  let rack = new Rack.rack "rack" in

  let river = new River.river "river" in
  
  let playground = new playground rack river in

  playground # on_event (event_handler wakener rack playground);

  let hand = [Some c1; Some c2; Some c3; Some c4; Some c5; Some c6; Some c7; Some c8; Some c9] in

  let exposed = [[Some rd; Some gd; Some wd]; [ Some ew; Some sw; Some ww; Some nw]] in

  let discard = [Some d1; Some d2; Some d3; Some d4; Some d5; Some d6; Some d7; Some d8; Some d9; Some b1; Some b2; Some b3; Some b4; Some b5; Some b6; Some b7; Some b8; Some b9] in

  for i = 0 to 3 do
    rack # set_hand i hand;
    rack # set_exposed i exposed;
    rack # set_discard i discard;
    rack # set_name i (Printf.sprintf " Player %i " i)
  done;

  LTerm_widget.run term ~save_state: true playground waiter
  
let () = Lwt_main.run gui
