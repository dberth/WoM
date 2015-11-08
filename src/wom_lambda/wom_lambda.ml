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
  
class playground (rack: Rack.rack) (river: River.river) (console: Console.console) =
  let event_listeners = ref [] in
  object (this)
    inherit t "playground"

    method wait_event =
      let waiter, wakener = Lwt.wait () in
      event_listeners := wakener :: !event_listeners;
      waiter

    method notify_event (event: LTerm_event.t) =
      let events = !event_listeners in
      event_listeners := [];
      List.iter (fun wakener -> Lwt.wakeup wakener event) events

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
        let side_col1 = cols - side_width - padding in
        let side_col2 = cols - padding in
        let river_top = rows - river # height - 1 in
        let console_rec = {row1 = 0; col1 = side_col1; row2 = river_top - 1; col2 = side_col2} in
        let river_rec = {row1 = river_top; col1 = side_col1; row2 = rows ; col2 = side_col2} in
        (* LTerm_draw.draw_frame ctx rack_rec LTerm_draw.Light; *)
        (* LTerm_draw.draw_frame ctx side_rec LTerm_draw.Light; *)
        rack # draw (LTerm_draw.sub ctx rack_rec) focused_widget;
        river # draw (LTerm_draw.sub ctx river_rec) focused_widget;
        console # draw (LTerm_draw.sub ctx console_rec) focused_widget;
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
        kind = AI {name = ""; force = 2};
      }

let get_initial_east_seat () = Lwt.return 0

let print_hand_explanations console hand_explanation =
  let open Lwt in
  return begin
    List.iter
      (fun (s, v) ->
         console # writeln (Printf.sprintf "%s: %.0f" s v)
      )
      hand_explanation
  end

let print_player_explanations game console score =
  let open Lwt in
  return begin
    for player = 0 to 3 do
      let s, v = Game_engine.explain_player_score game player score in
      let name = Game_engine.player_name game player in
      console # writeln (Printf.sprintf "%s: %0.f (%s)" name v s)
    done
  end

let print_current_scores game console =
  let open Lwt in
  return begin
    for player = 0 to 3 do
      let score = Game_engine.player_score game player in
      let name = Game_engine.player_name game player in
      console # writeln (Printf.sprintf "%s: %.0f" name score)
    done
  end

let new_round console river game =
  Lwt.return begin
    let prevailing_wind =
      match (Game_engine.nb_rounds game / 4) mod 4 with
      | 0 -> Common.East
      | 1 -> Common.South
      | 2 -> Common.West
      | 3 -> Common.North
      | _ -> assert false
    in
    console # writeln (Common.string_of_wind prevailing_wind);
    river # set_prevailing_wind (Some prevailing_wind)
  end

let end_game _ = Lwt.return ()

let throw_2_dice playground river =
  let open Lwt in
  let%lwt res1 = return (Random.int 6 + 1) in
  let%lwt res2 = return (Random.int 6 + 1) in
  return begin
    river # set_die_1 (Some res1);
    river # set_die_2 (Some res2);
    playground # queue_draw
  end >>
  Lwt_unix.sleep 1.5 >>
  return begin
    river # set_die_1 None;
    river # set_die_2 None;
    playground # queue_draw
  end >>
  Lwt_unix.sleep 0.2 >>
  Lwt.return (res1 + res2)

let human_event_loop game events playground rack console =
  let open Lwt in
  let open LTerm_event in
  let open LTerm_key in
  let open CamomileLibrary in
  let rec loop () =
    playground # wait_event >>= function
    | Key {code = Left; _} ->
      return begin
        rack # select_prev_event;
        playground # queue_draw
      end
      >> loop ()
    | Key {code = Right; _} ->
      return begin
        rack # select_next_event;
        playground # queue_draw;
      end
      >> loop ()
    | Key {code = Char c; _} ->
      begin match UChar.char_of c with
      | exception UChar.Out_of_range -> loop ()
      | ' ' ->
        begin match rack # selected_event with
          | None ->
            return (console # writeln "No selected event.") >>
            loop ()
          | Some event -> return event
        end
      | _ -> loop ()
      end
    | _ -> loop ()
  in
  loop ()
      
let player_of_gui_player game player =
  let east_seat = Game_engine.east_seat game in
  (player - east_seat + 4) mod 4

let gui_player_of_player game player =
  let east_seat = Game_engine.east_seat game in
  (player + east_seat) mod 4

let rec remove_first tile = function
  | [] -> []
  | hd :: tl when hd = tile -> tl
  | hd :: tl -> hd :: remove_first tile tl

let tileset_of_tile_pos_list console game tile_pos_list =
  let open Game_engine in
  let result =
    List.map
      (fun tile_pos ->
         match tile_of_tile_pos game tile_pos with
         | None -> assert false
         | Some tile -> tile
      )
      tile_pos_list
  in
  match discarded_tile game with
  | None -> result
  | Some discarded_tile -> remove_first discarded_tile result 

let human_move playground rack river console game events =
  let open Game_descr in
  let open Lwt in
  let event_tilesets =
    List.flatten
      (List.map
         (fun event ->
            match event with
            | Init _ | Wall_breaker_roll _ | Break_wall_roll _
            | Deal | Draw _ -> []
            | Discard (_, tile_pos)
            | Small_kong (_, tile_pos) ->
              begin match Game_engine.tile_of_tile_pos game tile_pos with
                | None -> assert false
                | Some tile -> [event, [tile]]
              end
            | Mahjong player ->
              let tiles =
                match Game_engine.hand game player with
                | None -> assert false
                | Some hand -> Tileset.tiles_of_tileset hand 
              in
              [event, tiles]
            | Concealed_kong (_, tile_pos_list)
            | Chow (_, tile_pos_list)
            | Pong (_, tile_pos_list)
            | Kong (_, tile_pos_list) ->
              [event, tileset_of_tile_pos_list console game tile_pos_list]
            | No_action _ -> [event, []]
              
         )
         events
      )
  in
  match event_tilesets with
  | [] ->
    begin match events with
    | [] -> assert false
    | event :: _ -> return event
    end
  (*| [No_action _ as event, []] -> return event*)
  | _ ->
    return begin
      rack # set_events event_tilesets;
      match Game_engine.last_drawn_tile game (player_of_gui_player game 0) with
      | None -> ()
      | Some tile -> rack # set_selected_tile tile
    end >>
    human_event_loop game events playground rack console

let player_wind game player =
  let open Common in
  match player_of_gui_player game player with
  | 0 -> East
  | 1 -> South
  | 2 -> West
  | 3 -> North
  | _ -> assert false
  

let set_river_winds river game =
  for player = 0 to 3 do
    river # set_seat_wind player (player_wind game player)
  done

let set_river_walls nb_tiles river game =
  river # set_nb_tiles_in_kong_box 14;
  let east_seat = Game_engine.east_seat game in
  let rotate x = (x - (east_seat * nb_tiles / 4) + nb_tiles) mod nb_tiles in
  let start = Game_engine.wall_start game in
  let last = Game_engine.last_tile game in
  match start, last with
  | None, _ | _, None -> ()
  | Some start, Some last ->
    river # set_wall_start (rotate start);
    river # set_last_tile (rotate last)

let set_river_tile river game =
  let discarded_tile = Game_engine.discarded_tile game in
  let discard_player = Game_engine.discard_player game in
  let tile =
    match discard_player, discarded_tile with
    | None, None -> None
    | Some player, Some tile -> Some (gui_player_of_player game player, tile)
    | _ -> assert false
  in
  river # set_tile tile

let set_river nb_tiles river game =
  let open Lwt in
  return (set_river_winds river game) >>
  return (set_river_walls nb_tiles river game) >>
  return (set_river_tile river game)

let set_rack_winds rack game =
  for player = 0 to 3 do
    rack # set_seat_wind player (player_wind game player)
  done

let set_rack_names rack game =
  for player = 0 to 3 do
    let player_name = Game_engine.player_name game player in
    rack # set_name player player_name
  done

let tiles_of_tileset player show_all tileset =
  List.map
    (fun tile ->
       if player = 0 || show_all then
         Some tile
       else
         None
    )
    (Tileset.tiles_of_tileset tileset)

let set_hand rack game show_all =
  for player = 0 to 3 do
    match Game_engine.hand game (player_of_gui_player game player) with
    | None -> rack # set_hand player []
    | Some tileset -> rack # set_hand player (tiles_of_tileset player show_all tileset)
  done

let mk_concealed = function
  | [_; t; _; _] -> [None; t; t; None]
  | _ -> assert false

let exposed_of_tilesets tilesets =
  List.map
    (fun (tileset, concealed) ->
       let tiles = tiles_of_tileset 0 true tileset in
       if concealed then
         mk_concealed tiles
       else
         tiles
    )
    tilesets

let set_exposed rack game =
  for player = 0 to 3 do
    let tilesets =
      Game_engine.exposed game (player_of_gui_player game player)
    in
    rack # set_exposed player (exposed_of_tilesets tilesets)
  done

let set_discarded rack game =
  for player = 0 to 3 do
    let tileset = Game_engine.discarded game (player_of_gui_player game player) in
    let tiles = List.rev_map (fun x -> Some x) tileset in
    rack # set_discard player tiles
  done

let set_rack rack game show_all_hands =
  let open Lwt in
  return (set_rack_winds rack game) >>
  return (set_rack_names rack game) >>
  return (set_hand rack game show_all_hands) >>
  return (set_exposed rack game) >>
  return (set_discarded rack game)

let on_game_event nb_tiles playground rack river event game =
  let open Lwt in
  set_rack rack game false >>
  set_river nb_tiles river game >>
  return (playground # queue_draw) >>
  Lwt.pause ()

let end_round_console console game =
  let open Lwt in
  
  if Game_engine.is_draw_game game then
    return (console # writeln "=== DRAW GAME ===")
  else begin
    let hand_explanation, score = Game_engine.explain_hand_score game in
    let hand_explanation = List.sort (fun (_, x) (_, y) -> compare x y) hand_explanation in
    return begin
      let name =
        match Game_engine.current_player_name game with
        | None -> assert false
        | Some name -> name
      in
      console # writeln
        (Printf.sprintf "=== %s WINS WITH %.0f PTS ==="
           name
           score
        )
    end >>
    print_hand_explanations console hand_explanation >>
    print_player_explanations game console score >>
    return (console # writeln "=== CURRENT SCORES ===") >>
    print_current_scores game console
  end

let end_round_rack playground rack game =
  set_rack rack game true >> Lwt.return (playground # queue_draw)

let end_round playground rack console game =
  let open Lwt in
  let show_info =
    end_round_rack playground rack game >> end_round_console console game
  in
  let wait =
    playground # wait_event >>= fun _ -> return ()
  in
  join  [show_info; wait]
  


let init nb_tiles playground rack river console =
  let roll () =
    throw_2_dice playground river
  in
  let callbacks =
    {
      Game_engine.get_rule;
      get_player_name;
      get_ai_player;
      get_initial_east_seat;
      wall_breaker_roll = roll;
      break_wall_roll = roll;
      human_move = human_move playground rack river console;
      end_round = end_round playground rack console;
      new_round = new_round console river;
      end_game;
      on_game_event = on_game_event nb_tiles playground rack river;
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
  | event -> playground # notify_event event; true
  (* | Key {code = Left; _} -> *)
  (*   rack # select_prev_tile; *)
  (*   playground # queue_draw; *)
  (*   true *)
  (* | Key {code = Right; _} -> *)
  (*   rack # select_next_tile; *)
  (*   playground # queue_draw; *)
  (*   true *)
  (* | _ -> false *)



let gui =
  let%lwt term = Lazy.force LTerm.stdout in
  let waiter, wakener = Lwt.wait () in
  
  let nb_tiles = 136 in

  let rack = new Rack.rack "rack" in

  let river = new River.river nb_tiles "river" in
  
  let console = new Console.console "console" in
  
  let playground = new playground rack river console in

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
  and () = init nb_tiles playground rack river console in
  Lwt.return ()
  
let () = Lwt_main.run gui
