(*Copyright (C) 2015 Denis Berthod*)

(*open Tileset*)
open LTerm_widget

let calculate_padding ctx ~rack_width ~side_width =
  let open LTerm_geom in
  let {cols; _} = LTerm_draw.size ctx in
  let remaining_space = cols - rack_width - side_width in
  if 30 < remaining_space then
    (remaining_space - 10) / 2
  else 
    let base = remaining_space / 3 in
    if remaining_space mod 3 = 2 then
      base + 1
    else
      base
  
class playground (rack: Rack.rack) (river: River.river) =
  object (this)
    inherit t "playground"

    method! draw ctx focused_widget =
      let open LTerm_geom in
      let {rows; cols} = LTerm_draw.size ctx in
      let side_width = river # width in

      let rack_rec = {row1 = 0; col1 = 0; row2 = rows; col2 = cols - side_width} in
      let rack_ctx = LTerm_draw.sub ctx rack_rec in
      match rack # width rack_ctx with
      | None -> LTerm_draw.draw_string ctx 0 0 "The screen is too small."
      | Some rack_width ->
        let padding = calculate_padding ctx ~rack_width ~side_width in
        let rack_rec = {rack_rec with col1 = padding; col2 = padding + rack_width } in
      let side_rec = {row1 = 0; col1 = cols - side_width - padding; row2 = rows ; col2 = cols - padding} in
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

let get_rule () =
  Lwt.return
    {
      Game_descr.name = "Zung Jung";
      flags = None;
    }

let get_player_name () = Lwt.return "Human"

let get_ai_player =
  let open Game_descr in
  let i = ref 0 in
  fun () ->
    incr i;
    Lwt.return
      {
        name = Printf.sprintf "CPU %i" !i;
        kind = AI {name = ""; force = 0};
      }

let get_initial_east_seat () = Lwt.return 0

let end_round _ = Lwt.return ()

let new_round _ = Lwt.return ()

let end_game _ = Lwt.return ()

let throw_2_dice playground river =
  let open Lwt in
  let res1 = Random.int 6 + 1 in
  let res2 = Random.int 6 + 1 in
  river # set_die_1 (Some res1);
  river # set_die_2 (Some res2);
  Lwt_io.printf "%i %i\n" res1 res2 >>
  Lwt_unix.sleep 1. >>
  (* Lwt_io.printf "End sleep\n" >> *)
  return (river # set_die_1 None) >>
  (* Lwt_io.printf "Die 1 reseted.\n" >> *)
  return (river # set_die_2 None) >>
  (* Lwt_io.printf "Die 2 reseted.\n" >> *)
  return (playground # queue_draw) >>
  Lwt.return (res1 + res2)

let human_move playground river _ events =
  let open Engine in
  let open Game_descr in
  let%lwt events =
    match events with
    | [Init [||]] -> Lwt.return [Init random_game]
    | [Wall_breaker_roll 0] ->
      let%lwt result = throw_2_dice playground river in
      Lwt.return [Wall_breaker_roll result] 
    | events -> Lwt.return events
  in
  let event = List.hd events in
  (* print_endline (Game_descr.string_of_round_event (event: Game_descr.round_event)); *)
  Lwt.return event

let on_game_event event _ = ()
(*print_endline (Game_descr.string_of_game_event event)*)

let init playground river =
    let callbacks =
      {
        Game_engine.get_rule;
        get_player_name;
        get_ai_player;
        get_initial_east_seat;
        human_move = human_move playground river;
        end_round;
        new_round;
        end_game;
        on_game_event;
      }
    in
    Random.self_init ();
    Game_engine.one_player_game_loop [] callbacks

  

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
  | Key {code = Left; _} ->
    rack # select_prev_tile;
    playground # queue_draw;
    true
  | Key {code = Right; _} ->
    rack # select_next_tile;
    playground # queue_draw;
    true
  | _ -> false



let gui =
  let%lwt term = Lazy.force LTerm.stdout in
  let waiter, wakener = Lwt.wait () in
  
  let rack = new Rack.rack "rack" in

  let river = new River.river 136 "river" in
  
  let playground = new playground rack river in

  playground # on_event (event_handler wakener rack playground);

  Rules.Loader.load_rules ();

  (* let hand = [Some c1; Some c2; Some c3; Some c4; Some c5; Some c6; Some c7; Some c8; Some c9] in *)

  (* let exposed = [[Some rd; Some gd; Some wd]; [ Some ew; Some sw; Some ww; Some nw]] in *)

  (* let discard = [Some d1; Some d2; Some d3; Some d4; Some d5; Some d6; Some d7; Some d8; Some d9; Some b1; Some b2; Some b3; Some b4; Some b5; Some b6; Some b7; Some b8; Some b9] in *)

  (* for i = 0 to 3 do *)
  (*   rack # set_hand i hand; *)
  (*   rack # set_exposed i exposed; *)
  (*   rack # set_discard i discard; *)
  (*   rack # set_name i (Printf.sprintf " Player %i " i) *)
  (* done; *)

  (* river # set_wall_start 20; *)
  (* river # set_last_tile 10; *)
  (* river # set_nb_tiles_in_kong_box 14; *)
  (* river # set_die_1 (Some 1); *)
  (* river # set_die_2 (None); *)
  (* river # set_tile (None); *)
  let%lwt () = LTerm_widget.run term ~save_state: true playground waiter
  and () = init playground river in
  Lwt.return ()
  
let () = Lwt_main.run gui
