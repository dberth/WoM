(*Copyright (C) 2014 Denis Berthod*)

module IntSet = Set.Make(struct type t = int let compare x y = x - y end)
open Tileset
open Fsm

type player = int

type tile_pos = int (*A position in the initial array*)

type declared = (tileset * tile_pos list * bool (*is_concealed*)) list

type mahjong =
  {
    declared: declared;
    hand: tileset;
    discard_player: player option;
    kong_robbing: bool
  }

type end_game =
  | No_winner
  | Mahjong of mahjong

type event =
  | Init of tile option array
  | Wall_breaker_roll of int
  | Break_wall_roll of int
  | Deal
  | Draw of player
  | Discard of player * tile_pos
  | Mahjong of player
  | Concealed_kong of player * tile_pos list
  | Small_kong of player * tile_pos
  | Chow of player * tile_pos list
  | Pong of player * tile_pos list
  | Kong of player * tile_pos list
  | No_action of player

exception Irrelevant_event of (event * string)

module TMap = Tileset.Map

type player_state =
  {
    hand: tileset;
    tiles_pos: int list TMap.t;
    semi_chows: (int * int) list TMap.t;
    possible_small_kongs: Tileset.tile list;
    declared: declared;
    discarded_tiles: int list;
  }


type game =
  {
    tiles: tile array;
    wall_breaker_roll: int;
    current_tile: int;
    last_tile: int;
    player_0: player_state;
    player_1: player_state;
    player_2: player_state;
    player_3: player_state;
    current_player: int;
    discarded_tile: int option;
    discard_player: int option;
    end_game: end_game option;
    discard_event: event option;
    small_kong_event: event option;
  }

let init_player =
  {
    hand = empty;
    tiles_pos = TMap.empty;
    semi_chows = TMap.empty;
    declared = [];
    discarded_tiles = [];
    possible_small_kongs = [];
  }

let init_game =
  {
    tiles = [||];
    wall_breaker_roll = 0;
    current_tile = 0;
    last_tile = 0;
    player_0 = init_player;
    player_1 = init_player;
    player_2 = init_player;
    player_3 = init_player;
    current_player = 0;
    discarded_tile = None;
    discard_player = None;
    end_game = None;
    discard_event = None;
    small_kong_event = None;
  }

let string_of_tileset tileset =
  Printf.sprintf "[%s]" (tile_descr_of_tileset tileset |> List.map string_of_tile_descr |> String.concat "; ")

let string_of_declared declared =
  String.concat " ;"
    (List.map
       (fun (tileset, _, concealed) ->
         let concealed = if concealed then "(concealed)" else "" in
         Printf.sprintf "%s%s" (string_of_tileset tileset) concealed
       )
       declared
    )

let string_of_mahjong {declared; hand; discard_player; kong_robbing} =
  Printf.sprintf "{\ndeclared: %s;\nhand: %s;\ndiscard_player: %s;\nkong_robbing: %b\n}"
    (string_of_declared declared)
    (string_of_tileset hand)
    (match discard_player with Some i -> string_of_int i | None -> "None")
    kong_robbing
    
let string_of_tile_index tiles idx = Printf.sprintf "%s(%i)" (string_of_tile_descr (tile_descr_of_tile (tiles.(idx)))) idx

let string_of_tile_indexes tiles indexes =
  List.map (string_of_tile_index tiles) indexes |>
    List.sort compare |> String.concat "; " |> Printf.sprintf "[%s]"

let string_of_event tiles = function
  | Init _ -> "Init"
  | Wall_breaker_roll i ->
    if i = 0 then "Wall_breaker_roll" else Printf.sprintf "Wall_breaker_roll(%i)" i
  | Break_wall_roll i ->
    if i = 0 then "Break_wall_roll" else Printf.sprintf "Break_wall_roll(%i)" i
  | Deal -> "Deal"
  | Draw player -> Printf.sprintf "Draw(%i)" player
  | Discard(player, tile_pos) -> Printf.sprintf "Discard(%i, %s)" player (string_of_tile_index tiles tile_pos)
  | Mahjong player -> Printf.sprintf "Mahjong(%i)" player
  | Concealed_kong (player, tiles_pos) -> Printf.sprintf "Concealed_kong(%i, %s)" player (string_of_tile_indexes tiles tiles_pos)
  | Small_kong (player, tile_pos) -> Printf.sprintf "Small_kong(%i, %s)" player (string_of_tile_index tiles tile_pos)
  | Chow (player, tiles_pos) -> Printf.sprintf "Chow(%i, %s)" player (string_of_tile_indexes tiles tiles_pos)
  | Pong (player, tiles_pos) -> Printf.sprintf "Pong(%i, %s)" player (string_of_tile_indexes tiles tiles_pos)
  | Kong (player, tiles_pos) -> Printf.sprintf "Kong(%i, %s)" player (string_of_tile_indexes tiles tiles_pos)
  | No_action player -> Printf.sprintf "No_action(%i)" player

let string_of_semi_chows semi_chows =
  TMap.fold
    (fun tile semi_chows acc ->
      ((Printf.sprintf "%s: " (string_of_tile_descr (tile_descr_of_tile tile))) ^
      (String.concat "; " (List.map (fun (x, y) -> Printf.sprintf "(%i, %i)" x y) semi_chows))) :: acc
    )
    semi_chows
    []
  |> String.concat " | "

let string_of_possible_small_kongs tiles =
  String.concat "; " (List.map string_of_tile tiles)

let string_of_player_state tiles {tiles_pos; semi_chows; declared; discarded_tiles; possible_small_kongs; hand = _} =
  Printf.sprintf "{\nhand: %s;\ndeclared: %s;\ndiscarded_tiles: %s; semi_chows: %s; possible small kongs: %s\n}"
    (string_of_tile_indexes tiles (List.concat (TMap.fold (fun _ positions acc -> positions :: acc) tiles_pos [])))
    (string_of_declared declared)
    (string_of_tile_indexes tiles discarded_tiles)
    (string_of_semi_chows semi_chows)
    (string_of_possible_small_kongs possible_small_kongs)

let string_of_end_game = function
  | No_winner -> "No_winner"
  | Mahjong mahjong -> string_of_mahjong mahjong

let string_of_game
    {
     tiles;
      wall_breaker_roll = _;
     current_tile;
      last_tile;
      player_0;
      player_1;
      player_2;
      player_3;
      current_player;
      discarded_tile;
      discard_player;
      end_game;
      discard_event;
      small_kong_event;
    } =
  Printf.sprintf "{\ncurrent_tile: %i;\n last_tile: %i;\n player_0:\n%s;\nplayer_1:\n%s;\nplayer_2:\n%s;\nplayer_3:\n%s;\ncurrent_player: %i;\n;\n discarded_tile: %s;\n discard_player: %s; end_game: %s;\n discard_event: %s;\n small_kong_event: %s;\n}"
    current_tile
    last_tile
    (string_of_player_state tiles player_0)
    (string_of_player_state tiles player_1)
    (string_of_player_state tiles player_2)
    (string_of_player_state tiles player_3)
    current_player
    (match discarded_tile with Some i -> string_of_int i | None -> "None")
    (match discard_player with Some i -> string_of_int i | None -> "None")
    (match end_game with Some end_game -> string_of_end_game end_game | None -> "None")
    (match discard_event with Some event -> string_of_event tiles event | None -> "None")
    (match small_kong_event with Some event -> string_of_event tiles event | None -> "None")

let string_of_event {tiles; _} event = string_of_event tiles event

let () =
  Printexc.register_printer
    (function Irrelevant_event (_event, s) ->
      Some (Printf.sprintf "Irrelevant event: %s" s)
      | _ -> None
    )

let update_player player f game =
  match player with
  | 0 -> {game with player_0 = f game.player_0}
  | 1 -> {game with player_1 = f game.player_1}
  | 2 -> {game with player_2 = f game.player_2}
  | 3 -> {game with player_3 = f game.player_3}
  | _ -> assert false

let update_game_from_player player f game =
  match player with
  | 0 -> f game.player_0
  | 1 -> f game.player_1
  | 2 -> f game.player_2
  | 3 -> f game.player_3
  | _ -> assert false

let init_tiles =
  [| b1; b1; b1; b1; b2; b2; b2; b2; b3; b3; b3; b3;
     b4; b4; b4; b4; b5; b5; b5; b5; b6; b6; b6; b6;
     b7; b7; b7; b7; b8; b8; b8; b8; b9; b9; b9; b9;

     d1; d1; d1; d1; d2; d2; d2; d2; d3; d3; d3; d3;
     d4; d4; d4; d4; d5; d5; d5; d5; d6; d6; d6; d6;
     d7; d7; d7; d7; d8; d8; d8; d8; d9; d9; d9; d9;

     c1; c1; c1; c1; c2; c2; c2; c2; c3; c3; c3; c3;
     c4; c4; c4; c4; c5; c5; c5; c5; c6; c6; c6; c6;
     c7; c7; c7; c7; c8; c8; c8; c8; c9; c9; c9; c9;

     gd; gd; gd; gd; rd; rd; rd; rd; wd; wd; wd; wd;

     ww; ww; ww; ww; nw; nw; nw; nw;
     ew; ew; ew; ew; sw; sw; sw; sw
  |]

let nb_tiles = Array.length init_tiles

module TSet = Tileset.Set

let init_tiles_set =
  Array.fold_right TSet.add init_tiles TSet.empty

let random_game = Array.make nb_tiles (None: tile option)

let shuffle_array arr =
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int i in
    let x = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- x
  done

let shuffle known_positions =
  let remaining_tiles =
    Array.fold_right
      (fun tile set ->
        match tile with
        | None -> set
        | Some tile -> TSet.remove tile set
      )
      known_positions
      init_tiles_set
  in
  let l = TSet.cardinal remaining_tiles in
  let arr = Array.make l d1 in
  let i = ref 0 in
  TSet.iter (fun tile -> arr.(!i) <- tile; incr i) remaining_tiles;
  shuffle_array arr;
  let result = Array.make nb_tiles d1 in
  let j = ref 0 in
  for i = 0 to nb_tiles - 1 do
    match known_positions.(i) with
    | None -> result.(i) <- arr.(!j); incr j
    | Some tile -> result.(i) <- tile
  done;
  result

let first_tile_index wall_breaker_roll break_wall_roll =
  let wall_breaker = (break_wall_roll - 1) mod 4 in
  let nb_tile_by_side = nb_tiles / 4 in
  let count_start_index = wall_breaker * nb_tile_by_side in
  (count_start_index + 2 * (wall_breaker_roll + break_wall_roll)) mod nb_tiles

let incr_current_tile game =
  {game with current_tile = (game.current_tile + 1) mod nb_tiles}

let decr_last_tile game =
  {game with last_tile = (game.last_tile + nb_tiles - 1) mod nb_tiles}

let rec insert_sorted x = function
  | [] -> [x]
  | (hd :: _ as l) when x = hd -> l
  | hd :: tl when x < hd -> hd :: (insert_sorted x tl)
  | l -> x :: l

let rec remove_sorted x = function
  | [] -> []
  | hd :: tl when x = hd -> tl
  | hd :: tl when x < hd -> hd :: (remove_sorted x tl)
  | l -> l

let add_tile_in_tiles_pos tile pos tiles_pos =
  TMap.update
    (function
      | None -> Some [pos]
      | Some positions -> Some (insert_sorted pos positions)
    )
    tile
    tiles_pos

let add_semi_chow tile semi_chow semi_chows =
  TMap.update
    (function
      | None -> Some [semi_chow]
      | Some semi_chows -> Some (insert_sorted semi_chow semi_chows)
    )
    tile
    semi_chows

let positions_of_tile tile tiles_pos =
  match tile with
  | None -> []
  | Some tile -> TMap.find_default tile [] tiles_pos

let set_semi_chows before positions key_tile pos semi_chows =
  List.fold_left
    (fun semi_chows posi ->
      let semi_chow =
        if before then posi, pos else pos, posi
      in
      match key_tile with
      | None -> semi_chows
      | Some tile ->
        add_semi_chow tile semi_chow semi_chows
    )
    semi_chows
    positions

let populate_semi_chows tile pos tiles_pos semi_chows =
  let pred_pred = Tileset.tile_pred_pred tile in
  let pred = Tileset.tile_pred tile in
  let succ = Tileset.tile_succ tile in
  let succ_succ = Tileset.tile_succ_succ tile in
  let pred_pred_pos = positions_of_tile pred_pred tiles_pos in
  let pred_pos = positions_of_tile pred tiles_pos in
  let succ_pos = positions_of_tile succ tiles_pos in
  let succ_succ_pos = positions_of_tile succ_succ tiles_pos in
  set_semi_chows true pred_pred_pos pred pos semi_chows |>
    set_semi_chows true pred_pos pred_pred pos |>
    set_semi_chows true pred_pos succ pos |>
    set_semi_chows false succ_pos pred pos |>
    set_semi_chows false succ_pos succ_succ pos |>
    set_semi_chows false succ_succ_pos succ pos
    
let set_tile player pos game =
  update_player player
    (fun player_state ->
      let  tile = game.tiles.(pos) in
      let hand = add_tile tile player_state.hand in
      let tiles_pos = add_tile_in_tiles_pos tile pos player_state.tiles_pos in 
      let semi_chows = populate_semi_chows tile pos tiles_pos player_state.semi_chows in
      {player_state with hand; tiles_pos; semi_chows}
    )
    game

let draw_tile player game =
  let game = incr_current_tile game in
  set_tile player game.current_tile game

let draw_last_tile player game =
  let game = decr_last_tile game in
  set_tile player game.last_tile game

let draw_4_tiles player game =
  draw_tile player game |>
    draw_tile player |>
    draw_tile player |>
    draw_tile player

let deal_turn f game = f 0 game |> f 1 |> f 2 |> f 3

let remove_pos_from_tiles_pos pos tile tiles_pos =
  TMap.update
    (function
      | None -> assert false
      | Some positions ->
        match remove_sorted pos positions with
        | [] -> None
        | x -> Some x
    )
    tile
    tiles_pos

let remove_pos_from_side_semi_chows pos key_tile semi_chows =
  match key_tile with
  | None -> semi_chows
  | Some key_tile ->
    TMap.update
      (function
      | None -> None
      | Some semi_chows ->
        let result =
          List.filter (fun (x, y) -> x <> pos && y <> pos) semi_chows
        in
        match result with
        | [] -> None
        | x -> Some x
      )
      key_tile
      semi_chows

let remove_pos_from_semi_chows pos tile semi_chows =
  remove_pos_from_side_semi_chows pos (Tileset.tile_pred_pred tile) semi_chows |>
    remove_pos_from_side_semi_chows pos (Tileset.tile_pred tile) |>
    remove_pos_from_side_semi_chows pos (Tileset.tile_succ tile) |>
    remove_pos_from_side_semi_chows pos (Tileset.tile_succ_succ tile)

let check_player player event game =
  if player = game.current_player then
    game
  else
    raise (Irrelevant_event(event, Printf.sprintf "Expected player was %i." game.current_player))

let remove_tile_from_hand tile_idx tiles event (player_state: player_state) =
  let tile = tiles.(tile_idx) in
  begin match remove_tile tile player_state.hand with
  | hand ->
    let tiles_pos = remove_pos_from_tiles_pos tile_idx tile player_state.tiles_pos in
    let semi_chows = remove_pos_from_semi_chows tile_idx tile player_state.semi_chows in
    {player_state with hand; tiles_pos; semi_chows}
  | exception Not_found ->
    raise (Irrelevant_event (event, "No such tile in player hand."))
  end

let next_player player = (player + 1) mod 4

let prev_player player = (player + 3) mod 4

let prev_prev_player player = (player + 2) mod 4

let discard player tile_idx event game =
  {game with
    discarded_tile = Some tile_idx;
    discard_player = Some player;
    current_player = next_player player
  } |>
    update_player player (remove_tile_from_hand tile_idx game.tiles event)

let mahjong ~discard_player ?(kong_robbing = false) player game =
  update_game_from_player player
    (fun {hand; declared; _} ->
      let hand =
        match game.discarded_tile with
        | None -> hand
        | Some discarded_tile -> Tileset.add_tile (game.tiles.(discarded_tile)) hand
      in
      {game with end_game = Some (Mahjong {declared; hand; discard_player; kong_robbing})}
    )
    game

let mk_tileset_of_tiles_pos tiles tiles_pos =
  List.fold_left
    (fun tileset tile_pos -> add_tile tiles.(tile_pos) tileset)
    empty
    tiles_pos

let declare_tileset ~is_pong ~concealed player tiles_pos event game =
  update_player player
    (fun player_state ->
      let player_state =
        List.fold_left
          (fun player_state tile_idx ->
            let res = remove_tile_from_hand tile_idx game.tiles event player_state in
            res
          )
          player_state
          tiles_pos
      in
      let tileset = mk_tileset_of_tiles_pos game.tiles tiles_pos in
      let possible_small_kongs =
        if is_pong then
          game.tiles.(List.hd tiles_pos) :: player_state.possible_small_kongs
        else
          player_state.possible_small_kongs
      in
      {player_state with declared = (tileset, tiles_pos, concealed) :: player_state.declared; possible_small_kongs}
    )
    game

let declare_concealed_kong player tiles_pos event game =
  declare_tileset ~is_pong: false ~concealed: true player tiles_pos event game

let set_small_kong tile_pos tiles event player_state =
  let rec aux = function
    | [] -> raise (Irrelevant_event(event, "Cannot find pong to make a small kong."))
    | (tileset, tiles_pos, concealed as x) :: tl ->
      if List.length tiles_pos = 4 then
        x :: aux tl
      else
        let tileset = add_tile tiles.(tile_pos) tileset in
        if is_kong tileset then
          (tileset, tile_pos :: tiles_pos, concealed) :: tl
        else
          x :: aux tl
  in
  {player_state with declared = aux player_state.declared}

let declare_small_kong game =
  match game.small_kong_event with
  | Some (Small_kong(player, tile_pos) as event) ->
    {game with small_kong_event = None} |>
      update_player player
        (fun player_state ->
          remove_tile_from_hand tile_pos game.tiles event player_state |>
            set_small_kong tile_pos game.tiles event
        )
  | _ -> assert false
      

let set_discarded_tile player tiles_pos event game =
  match game.discarded_tile with
  | None -> assert false
  | Some discarded_tile ->
    if List.mem discarded_tile tiles_pos then begin
      {game with discarded_tile = None; discard_player = None; current_player = player} |>
        set_tile player discarded_tile
    end else
      raise (Irrelevant_event(event, "Event doesn't concern discarded tile."))

let declare_discarded_tileset ~is_pong player tiles_pos event game =
  set_discarded_tile player tiles_pos event game |>
    declare_tileset ~is_pong ~concealed: false player tiles_pos event

let set_discarded_tile player game =
  match game.discarded_tile with
  | None -> assert false
  | Some discarded_tile ->
    { game with
      current_player = next_player player;
      discarded_tile = None;
      discard_player = None;
    } |> update_player player
        (fun player_state ->
          {player_state with
            discarded_tiles = discarded_tile :: player_state.discarded_tiles
          }
        )

let player_state player {player_0; player_1; player_2; player_3; _} =
  match player with
  | 0 -> player_0
  | 1 -> player_1
  | 2 -> player_2
  | 3 -> player_3
  | _ -> assert false

let current_player_state game = player_state game.current_player game

let get_discarded_tile_pos game =
  match game.discarded_tile with
  | None -> assert false
  | Some tile_pos -> tile_pos

let get_discarded_tile game =
  game.tiles.(get_discarded_tile_pos game)

let hand_with_discarded_tile game =
  let {hand; _} = current_player_state game in
  let tile = get_discarded_tile game in
  add_tile tile hand

let hand_with_kr_tile game =
  match game.small_kong_event with
  | Some (Small_kong(_, tile_pos)) ->
    add_tile (game.tiles.(tile_pos)) (current_player_state game).hand
  | _ -> assert false

let mahjong_event hand_with_other_tile ~seven_pairs ?irregular_hands game =
  let player_state = current_player_state game in
  match Tileset.mahjong ~seven_pairs ?irregular_hands (4 - List.length player_state.declared) (hand_with_other_tile game) with
  | [] -> []
  | _ -> [Mahjong game.current_player]

let kr_mahjong_event ?irregular_hands game =
  mahjong_event hand_with_kr_tile ?irregular_hands game

let mahjong_event ?irregular_hands game =
  mahjong_event hand_with_discarded_tile ?irregular_hands game

let rec take n l =
  if n = 0 then
    []
  else
    match l with
    | [] -> []
    | hd :: tl -> hd :: take (n - 1) tl

let discard_events {tiles_pos; _} game =
  TMap.fold
    (fun _ positions acc ->
      match positions with
      | [] -> acc
      | hd :: _ -> Discard (game.current_player, hd) :: acc
    )
    tiles_pos
    []

let pong_and_kong_events game =
  let pos =
    match game.discarded_tile with
    | None -> assert false
    | Some pos -> pos
  in
  let {tiles_pos; _} = current_player_state game in
  let tile = game.tiles.(pos) in
  let positions = TMap.find_default tile [] tiles_pos in
  let l = List.length positions in
  if l = 3 then
    [Pong (game.current_player, insert_sorted pos (take 2 positions));
     Kong (game.current_player, insert_sorted pos positions)
    ]
  else if l = 2 then
    [Pong (game.current_player, insert_sorted pos positions)]
  else
    []

let concealed_kong_events {tiles_pos; _} game =
  TMap.fold
    (fun _ positions acc ->
      if List.length positions = 4 then
        Concealed_kong(game.current_player, positions) :: acc
      else
        acc
    )
    tiles_pos
    []

let no_action_event game = [No_action game.current_player]

(*** Actions ***)

let on_game_start_exit event game =
  match event with
  | Init known_positions ->
    let tiles = shuffle known_positions in
    {game with tiles;}
  | _ -> assert false

let on_wait_for_wall_breaker_roll_exit event game =
  match event with
  | Wall_breaker_roll wall_breaker_roll ->
    {game with wall_breaker_roll}
  | _ -> assert false

let on_wait_for_break_roll_exit event game =
  match event with
  | Break_wall_roll dice ->
    let current_tile = first_tile_index game.wall_breaker_roll dice in 
    {game with
      current_tile;
      last_tile = (current_tile + nb_tiles - 1) mod nb_tiles;
    }
  | _ -> assert false

let on_wait_for_deal_exit event game =
  match event with
  | Deal ->
    deal_turn draw_4_tiles game |>
      deal_turn draw_4_tiles |>
      deal_turn draw_4_tiles |>
      deal_turn draw_tile
  | _ -> assert false

let on_wait_for_draw_in_wall_entry _ ({current_tile; last_tile; _} as game) =
  let remaining_tiles =
    if last_tile < current_tile then
      current_tile + nb_tiles - last_tile + 1
    else
      last_tile - current_tile + 1
  in
  if remaining_tiles < 14 then
    {game with end_game = Some No_winner}
  else
    game

let on_player_turn_entry _ game = {game with discard_event = None}

let on_wait_for_draw_in_wall_exit event game =
  match event with
  | Draw player ->
    check_player player event game |> draw_tile player
  | _ -> assert false

let on_player_turn_exit event game =
  match event with
  | Discard (player, tile_idx) ->
    check_player player event game |> discard player tile_idx event
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: None player (*Maybe do this on mahjong declared state ?*)
  | Concealed_kong (player, tiles_pos) ->
    check_player player event game |>
      declare_concealed_kong player tiles_pos event 
  | Small_kong (player, tile_pos) ->
    let game = check_player player event game in
    {game with small_kong_event = Some event; current_player = next_player player}
  | _ -> assert false


let on_tile_discarded_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (prev_player player)) player
  | No_action player
  | Chow (player, _)
  | Pong (player, _)
  | Kong (player, _) ->
    let game = check_player player event game in
    {game with
      current_player = next_player player;
      discard_event = Some event;
    }
  | _ -> assert false

let on_td_1_no_action_2_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (prev_prev_player player)) player
  | No_action player ->
    let game = check_player player event game in
    {game with current_player = next_player player}
  | Pong (player, _) | Kong (player, _) ->
    let game = check_player player event game in
    {game with
      current_player = next_player player;
      discard_event = Some event;
    }
  | _ -> assert false

let on_td_1_chow_2_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (prev_prev_player player)) player
  | No_action player ->
    let game = check_player player event game in
    {game with current_player = next_player player}
  | Pong (player, _) | Kong (player, _) ->
    let game = check_player player event game in
    {game with
      current_player = next_player player;
      discard_event = Some event;
    }
  | _ -> assert false

let on_td_1_pong_2_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (prev_prev_player player)) player
  | No_action player ->
    let game = check_player player event game in
    {game with current_player = next_player player}
  | _ -> assert false

let on_td_1_kong_2_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (prev_prev_player player)) player
  | No_action player ->
    let game = check_player player event game in
    {game with current_player = next_player player}
  | _ -> assert false

let on_td_1_no_action_3_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (next_player player)) player
  | No_action player -> set_discarded_tile (next_player player) game
  | Pong (player, tiles_pos) -> declare_discarded_tileset ~is_pong: true player tiles_pos event game
  | Kong (player, tiles_pos) -> declare_discarded_tileset ~is_pong: false player tiles_pos event game
  | _ -> assert false

let on_td_2_pong_3_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (next_player player)) player
  | No_action _ ->
    begin match game.discard_event with
    | Some (Pong(player, tiles_pos)) -> declare_discarded_tileset ~is_pong: true player tiles_pos event game
    | _ -> assert false
    end
  | _ -> assert false

let on_td_2_kong_3_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (next_player player)) player
  | No_action _ ->
    begin match game.discard_event with
    | Some(Kong(player, tiles_pos)) -> declare_discarded_tileset ~is_pong: false player tiles_pos event game
    | _ -> assert false
    end
  | _ -> assert false


let on_td_1_chow_3_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (next_player player)) player
  | No_action _->
    begin match game.discard_event with
    | Some(Chow(player, tiles_pos)) -> declare_discarded_tileset ~is_pong: false player tiles_pos event game
    | _ -> assert false
    end
  | Pong (player, tiles_pos) -> declare_discarded_tileset ~is_pong: true player tiles_pos event game
  | Kong (player, tiles_pos) -> declare_discarded_tileset ~is_pong: false player tiles_pos event game
  | _ -> assert false

let on_kong_declared_exit event game =
  match event with
  | Draw player -> check_player player event game |> draw_last_tile player
  | _ -> assert false
  
let on_wait_for_kong_robbing_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (prev_player player)) ~kong_robbing: true player
  | No_action player ->
    {game with current_player = next_player player}
  | _ -> assert false

let on_kr_2_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (prev_prev_player player)) ~kong_robbing: true player
  | No_action player ->
    {game with current_player = next_player player}
  | _ -> assert false

let on_kr_3_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (next_player player)) ~kong_robbing: true player
  | No_action _-> declare_small_kong game
  | _ -> assert false


let build_engine ~seven_pairs ?irregular_hands events = 
  let rec game_start = lazy (new_state
    ~accepted_events: (fun _ -> [Init [||]])
      (function
      | Init _ -> wait_for_wall_breaker_roll
      | event -> raise (Irrelevant_event (event, "game_start"))))

  and wait_for_wall_breaker_roll = lazy (new_state
    ~accepted_events: (fun _ -> [Wall_breaker_roll 0])
      (function
      | Wall_breaker_roll x when (1 < x && x <= 12) -> wait_for_break_roll
      | event -> raise (Irrelevant_event (event, "wait_for_wall_break_roll"))))

  and wait_for_break_roll = lazy (new_state
    ~accepted_events: (fun _ -> [Break_wall_roll 0])
      (function
      | Break_wall_roll x when (1 < x && x <= 12) -> wait_for_deal
      | event -> raise (Irrelevant_event (event, "wait_for_break_roll"))))

  and wait_for_deal = lazy (new_state
    ~accepted_events: (fun _ -> [Deal])
      (function
      | Deal -> wait_for_draw_in_wall
      | event -> raise (Irrelevant_event (event, "wait_for_deal"))))
    
  and wait_for_draw_in_wall = lazy (new_state
    ~accepted_events: (fun game -> [Draw (game.current_player)])
      (function
      | Draw _ -> player_turn
      | event -> raise (Irrelevant_event (event, "wait_for_draw_in_wall"))))

  and player_turn =
    let accepted_events game =
      let player_state = current_player_state game in
      let discard_events = discard_events player_state game in
      let mahjong_event =
        match Tileset.mahjong ~seven_pairs ?irregular_hands (4 - List.length player_state.declared) player_state.hand with
        | [] -> []
        | _ -> [Mahjong game.current_player]
      in
      let concealed_kong_events = concealed_kong_events player_state game in
      let small_kong_events =
        List.fold_left
          (fun acc tile ->
            match TMap.find_default tile [] player_state.tiles_pos with
            | [] -> acc
            | [pos] -> Small_kong (game.current_player, pos) :: acc
            | x ->
              print_endline (String.concat "; " (List.map (fun tile -> string_of_tile_descr (tile_descr_of_tile tile)) player_state.possible_small_kongs));
              print_endline (String.concat "; " (List.map string_of_int x));
              print_endline (string_of_declared player_state.declared);
              print_endline (String.concat " | " (List.map (fun (_, positions, _) -> String.concat "; " (List.map string_of_int positions)) player_state.declared));
              assert false
          )
          []
          player_state.possible_small_kongs
      in
      discard_events @ mahjong_event @ concealed_kong_events @ small_kong_events
    in
    lazy (new_state
      ~accepted_events
        (function
        | Discard _ -> tile_discarded
        | Mahjong _ -> mahjong_declared
        | Concealed_kong _ -> kong_declared
        | Small_kong _ -> wait_for_kong_robbing
        | event -> raise (Irrelevant_event (event, "player_turn"))))

  and tile_discarded =
    let accepted_events game =
      let discarded_pos =
        match game.discarded_tile with
        | None -> assert false
        | Some pos -> pos
      in
      let player_state = current_player_state game in
      let chow_events =
        let tile = game.tiles.(discarded_pos) in
        List.map
          (fun (x, y) ->
            Chow (game.current_player, List.sort (-) [discarded_pos; x; y])
          )
          (TMap.find_default tile [] player_state.semi_chows)
      in
      no_action_event game @ mahjong_event ~seven_pairs ?irregular_hands game @ chow_events @ pong_and_kong_events game
    in
    lazy (new_state
        ~accepted_events
        (function
        | No_action _ -> td_1_no_action_2
        | Mahjong _ -> mahjong_declared
        | Chow _ -> td_1_chow_2
        | Pong _ -> td_1_pong_2
        | Kong _ -> td_1_kong_2
        | event -> raise (Irrelevant_event (event, "tile_discarded"))))

  and td_1_no_action_2 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
         pong_and_kong_events;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | No_action _ -> td_1_no_action_3
        | Mahjong _ -> mahjong_declared
        | Pong _ -> td_2_pong_3
        | Kong _ -> td_2_kong_3
        | event -> raise (Irrelevant_event (event, "1_no_action_2"))))

  and td_1_chow_2 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
         pong_and_kong_events;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | No_action _ -> td_1_chow_3
        | Mahjong _ -> mahjong_declared
        | Pong _ -> td_2_pong_3
        | Kong _ -> td_2_kong_3
        | event -> raise (Irrelevant_event (event, "1_chow_2"))))

  and td_1_pong_2 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
      ~accepted_events
      (function
      | No_action _ -> td_1_pong_3
      | Mahjong _ -> mahjong_declared
      | event -> raise (Irrelevant_event (event, "1_pong_2"))))

  and td_1_kong_2 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | No_action _ -> td_1_kong_3
        | Mahjong _ -> mahjong_declared
        | event -> raise (Irrelevant_event (event, "1_kong_2"))))

  and td_1_no_action_3 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
         pong_and_kong_events;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | No_action _ -> wait_for_draw_in_wall
        | Mahjong _ -> mahjong_declared
        | Pong _ -> player_turn
        | Kong _ -> kong_declared
        | event -> raise (Irrelevant_event (event, "1_no_action_3"))))

  and td_2_pong_3 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
        ~accepted_events
        (function
        | No_action _ -> player_turn
        | Mahjong _ -> mahjong_declared
        | event -> raise (Irrelevant_event (event, "2_pong_3"))))

  and td_2_kong_3 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
        ~accepted_events
        (function
        | No_action _ -> kong_declared
        | Mahjong _ -> mahjong_declared
        | event -> raise (Irrelevant_event (event, "2_kong_3"))))

  and td_1_chow_3 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
         pong_and_kong_events;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | No_action _ -> player_turn
        | Mahjong _ -> mahjong_declared
        | Pong _ -> player_turn
        | Kong _ -> kong_declared
        | event -> raise (Irrelevant_event (event, "1_chow_3"))))

  and td_1_pong_3 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | No_action _ -> player_turn
        | Mahjong _ -> mahjong_declared
        | event -> raise (Irrelevant_event (event, "1_pong_3"))))

  and td_1_kong_3 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | No_action _ -> kong_declared
        | Mahjong _ -> mahjong_declared
        | event -> raise (Irrelevant_event (event, "1_kong_3"))))

  and mahjong_declared = lazy (new_state (fun _ -> mahjong_declared))

  and kong_declared =
    lazy (new_state
      ~accepted_events: (fun game -> [Draw game.current_player])
        (function
        | Draw _ -> player_turn
        | event -> raise (Irrelevant_event (event, "kong_declared"))))

  and wait_for_kong_robbing =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         kr_mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | Mahjong _ -> mahjong_declared
        | No_action _ -> kr_2
        | event -> raise (Irrelevant_event (event, "wait_for_kong_robbing"))))

  and kr_2 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         kr_mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | Mahjong _ -> mahjong_declared
        | No_action _ -> kr_3
        | event -> raise (Irrelevant_event (event, "kr_2"))))

  and kr_3 =
    let accepted_events game =
      List.fold_left (fun acc f -> f game @ acc)
        []
        [no_action_event;
         kr_mahjong_event ~seven_pairs ?irregular_hands;
        ]
    in
    lazy (new_state
      ~accepted_events
        (function
        | Mahjong _ -> mahjong_declared
        | No_action _ -> kong_declared
        | event -> raise (Irrelevant_event (event, "kr_3"))))
  in
  let action_handler =
    empty_action_handler |>
      on_entry wait_for_draw_in_wall on_wait_for_draw_in_wall_entry |>
      on_entry player_turn on_player_turn_entry |>
      on_exit game_start on_game_start_exit |>
      on_exit wait_for_wall_breaker_roll on_wait_for_wall_breaker_roll_exit |>
      on_exit wait_for_break_roll on_wait_for_break_roll_exit |>
      on_exit wait_for_deal on_wait_for_deal_exit |>
      on_exit wait_for_draw_in_wall on_wait_for_draw_in_wall_exit |>
      on_exit player_turn on_player_turn_exit |>
      on_exit tile_discarded on_tile_discarded_exit |>
      on_exit td_1_no_action_2 on_td_1_no_action_2_exit |>
      on_exit td_1_chow_2 on_td_1_chow_2_exit |>
      on_exit td_1_pong_2 on_td_1_pong_2_exit |>
      on_exit td_1_kong_2 on_td_1_kong_2_exit |>
      on_exit td_1_no_action_3 on_td_1_no_action_3_exit |>
      on_exit td_2_pong_3 on_td_2_pong_3_exit |>
      on_exit td_2_kong_3 on_td_2_kong_3_exit |>
      on_exit td_1_chow_3 on_td_1_chow_3_exit |>
      on_exit td_1_pong_3 on_td_2_pong_3_exit |>
      on_exit td_1_kong_3 on_td_2_kong_3_exit |>
      on_exit kong_declared on_kong_declared_exit |>
      on_exit wait_for_kong_robbing on_wait_for_kong_robbing_exit |>
      on_exit kr_2 on_kr_2_exit |>
      on_exit kr_3 on_kr_3_exit

  in
  let world, state = run action_handler init_game game_start events in
  action_handler, world, state

let finished {end_game; _} = end_game

let set_known_tile tiles known_tiles tile_pos =
  known_tiles.(tile_pos) <- Some (tiles.(tile_pos))

let set_player_known_tiles ~viewer player tiles player_state known_tiles =
  if viewer = player then begin
    TMap.iter
      (fun tile positions ->
        List.iter
          (fun pos -> known_tiles.(pos) <- Some tile)
          positions
      )
      player_state.tiles_pos
  end;
  List.iter
    (fun (_, tiles_pos, _) ->
      List.iter (set_known_tile tiles known_tiles) tiles_pos
    )
    player_state.declared;
  List.iter (set_known_tile tiles known_tiles) player_state.discarded_tiles
      
  

let known_tiles {current_player; tiles; player_0; player_1; player_2; player_3; discarded_tile; _} =
  let viewer = current_player in
  let known_tiles = Array.make (Array.length tiles) None in
  set_player_known_tiles ~viewer 0 tiles player_0 known_tiles;
  set_player_known_tiles ~viewer 1 tiles player_1 known_tiles;
  set_player_known_tiles ~viewer 2 tiles player_2 known_tiles;
  set_player_known_tiles ~viewer 3 tiles player_3 known_tiles;
  begin match discarded_tile with
  | None -> ()
  | Some pos -> set_known_tile tiles known_tiles pos
  end;
  known_tiles

let current_player {current_player; _} = current_player

let current_player_hand game =
  let {hand; _} = current_player_state game in
  hand

let player_hand player game =
  let {hand; _} = player_state player game in
  hand

let player_declared_sets player game =
  let {declared; _} = player_state player game in
  declared

let player_discarded_tiles player game =
  let {discarded_tiles; _} = player_state player game in
  List.fold_right
    (fun i acc ->
      game.tiles.(i) :: acc
    )
    discarded_tiles
    []

let nb_tiles_in_hand player game =
  TMap.fold
    (fun tile positions acc ->
       acc + List.length positions
    )
    (player_state player game).tiles_pos
    0

let tile_of_tile_pos {tiles; _} pos = tiles.(pos)

let descr_of_tile_pos game pos = tile_descr_of_tile (tile_of_tile_pos game pos)

let rec is_in_declared pos = function
  | [] -> false
  | (_, positions, _) :: tl ->
    if List.mem pos positions then
      true
    else
      is_in_declared pos tl

let is_in_current_player_hand game pos =
  let {declared; tiles_pos; _} = current_player_state game in
  if is_in_declared pos declared then
    true
  else
    TMap.fold
      (fun _ positions acc ->
        if acc then acc else
          List.mem pos positions
      )
      tiles_pos
      false

let discarded_tile game =
  match game.discarded_tile with
  | None -> None
  | Some pos -> Some (game.tiles.(pos))

let discard_player {discard_player; _} = discard_player
