(* Copyright (C) 2014 Denis Berthod*)

open Tileset
open Engine

module IntSet = Set.Make(struct type t = int let compare = (-) end)

type declared = (tileset * bool (*is_concealed*)) list

type mahjong =
  {
    declared: declared;
    hand: tileset;
    discard_player: int option;
  }

type player_state =
  {
    hand: tileset;
    hand_indexes: IntSet.t;
    declared: declared;
  }

type end_game =
  | No_winner
  | Mahjong of mahjong

type game =
  {
    history: event list;
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
    end_game: end_game option;
    discard_event: event option;
  }

let init_player =
  {
    hand = empty;
    hand_indexes = IntSet.empty;
    declared = [];
  }

let init_game =
  {
    history = [];
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
    end_game = None;
    discard_event = None;
  }

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

let random_game = Array.make nb_tiles (None: tile_descr option)

let init_tile_descrs = Array.map tile_descr_of_tile init_tiles

let find_index tile_descr tile_descrs =
  let i = ref 0 in
  let l = Array.length tile_descrs in
  while !i < l && tile_descrs.(!i) <> tile_descr do incr i done;
  !i

let find_free_backward i known_positions =
  let i = ref i in
  while 0 <= !i && known_positions.(!i) <> None do decr i done;
  if !i = (-1) then
    None
  else
    Some !i

let find_free_forward i known_positions =
  let i = ref i in
  let l = Array.length known_positions in
  while !i < l && known_positions.(!i) <> None do incr i done;
  if !i = l then
    None
  else
    Some !i

let find_swap_index i known_positions tile_descrs =
  match known_positions.(i) with
  | Some tile_desc -> find_index tile_desc tile_descrs
  | None ->
    let j = Random.int (i + 1) in
    match known_positions.(j) with
    | None -> j
    | Some _ ->
      match find_free_backward j known_positions with
      | Some k -> k
      | None ->
        match find_free_forward j known_positions with
        | Some k -> k
        | None -> assert false

let swap arr i j =
  let x = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- x

let shuffle known_positions =
  let tiles = Array.copy init_tiles in
  let tile_descrs = Array.copy init_tile_descrs in
  assert (Array.length tiles = Array.length known_positions);
  Random.self_init ();
  for i = Array.length tiles - 1 downto 0 do
    let j = find_swap_index i known_positions tile_descrs in
    swap tiles i j;
    swap tile_descrs i j
  done;
  tiles

let first_tile_index wall_breaker_roll break_wall_roll =
  let wall_breaker = (break_wall_roll - 1) mod 4 in
  let nb_tile_by_side = nb_tiles / 4 in
  let count_start_index = wall_breaker * nb_tile_by_side in
  (count_start_index + 2 * (wall_breaker_roll + break_wall_roll)) mod nb_tiles

let incr_current_tile game =
  {game with current_tile = (game.current_tile + 1) mod nb_tiles}

let draw_tile player game =
  let game = incr_current_tile game in
  update_player player
    (fun player_state ->
      let hand = add_tile (game.tiles.(game.current_tile)) player_state.hand in
      let hand_indexes = IntSet.add game.current_tile player_state.hand_indexes in
      {player_state with hand; hand_indexes}
    )
    game

let draw_4_tiles player game =
  draw_tile player game |>
    draw_tile player |>
    draw_tile player |>
    draw_tile player

let deal_turn f game = f 0 game |> f 1 |> f 2 |> f 3

let check_player player event game =
  if player = game.current_player then
    game
  else
    raise (Irrelevant_event(event, Printf.sprintf "Expected player was %i." game.current_player))

let remove_tile_from_hand tile_idx tiles event (player_state: player_state) =
  begin match remove_tile tiles.(tile_idx) player_state.hand with
  | hand ->
    let hand_indexes = IntSet.remove tile_idx player_state.hand_indexes in
    {player_state with hand; hand_indexes}
  | exception Not_found ->
    raise (Irrelevant_event (event, "No such tile in player hand."))
  end

let next_player player = player + 1 mod 4

let prev_player player = player + 3 mod 4

let discard player tile_idx event game =
  {game with
    discarded_tile = Some tile_idx;
    current_player = next_player player
  } |>
    update_player player (remove_tile_from_hand tile_idx game.tiles event)

let mahjong ~discard_player player game =
  update_game_from_player player
    (fun {hand; declared; _} ->
      {game with end_game = Some (Mahjong {declared; hand; discard_player})}
    )
    game

let mk_tileset_of_tiles_pos tiles tiles_pos =
  List.fold_left
    (fun tileset tile_pos -> add_tile tiles.(tile_pos) tileset)
    empty
    tiles_pos

let declare_tileset ~concealed player tiles_pos event game =
  update_player player
    (fun player_state ->
      let player_state =
        List.fold_left
          (fun player_state tile_idx ->
            remove_tile_from_hand tile_idx game.tiles event player_state
          )
          player_state
          tiles_pos
      in
      let tileset = mk_tileset_of_tiles_pos game.tiles tiles_pos in
      {player_state with declared = (tileset, concealed) :: player_state.declared}
    )
    game

let declare_concealed_kong player tiles_pos event game =
  declare_tileset ~concealed: true player tiles_pos event game

let set_small_kong tile_pos tiles event player_state =
  let rec aux = function
    | [] -> raise (Irrelevant_event(event, "Cannot find pong to make a small kong."))
    | (tileset, concealed as x) :: tl ->
      if is_kong tileset then
        x :: aux tl
      else
        let tileset = add_tile tiles.(tile_pos) tileset in
        if is_kong tileset then
          (tileset, concealed) :: tl
        else
          x :: aux tl
  in
  {player_state with declared = aux player_state.declared}

let declare_small_kong player tile_pos event game =
  update_player player
    (fun player_state ->
      remove_tile_from_hand tile_pos game.tiles event player_state |>
        set_small_kong tile_pos game.tiles event
    )
    game

(*** Actions ***)

let on_game_start_exit event game =
  match event with
  | Init known_positions ->
    let tiles = shuffle known_positions in
    let init_event = Array.map (fun x -> Some (tile_descr_of_tile x)) tiles in
    {game with history = [Init init_event]; tiles;}
  | _ -> assert false

let on_wait_for_wall_breaker_roll_exit event game =
  match event with
  | Wall_breaker_roll wall_breaker_roll ->
    {game with history = event :: game.history; wall_breaker_roll}
  | _ -> assert false

let on_wait_for_break_roll_exit event game =
  match event with
  | Break_wall_roll dice ->
    let current_tile = first_tile_index game.wall_breaker_roll dice in 
    {game with
      history = event :: game.history;
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
    check_player player event game |>
      declare_small_kong player tile_pos event
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
      mahjong ~discard_player: (Some (player |> prev_player |> prev_player)) player
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
      mahjong ~discard_player: (Some (player |> prev_player |> prev_player)) player
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
      mahjong ~discard_player: (Some (player |> prev_player |> prev_player)) player
  | No_action player ->
    let game = check_player player event game in
    {game with current_player = next_player player}
  | _ -> assert false

let on_td_1_kong_2_exit (event: event) game =
  match event with
  | Mahjong player ->
    check_player player event game |>
      mahjong ~discard_player: (Some (player |> prev_player |> prev_player)) player
  | No_action player ->
    let game = check_player player event game in
    {game with current_player = next_player player}
  | _ -> assert false


let run_game =
  build_engine
    ~on_game_start_exit
    ~on_wait_for_wall_breaker_roll_exit
    ~on_wait_for_break_roll_exit
    ~on_wait_for_deal_exit
    ~on_wait_for_draw_in_wall_exit
    ~on_player_turn_exit
    ~on_tile_discarded_exit
    ~on_td_1_no_action_2_exit
    ~on_td_1_chow_2_exit
    ~on_td_1_pong_2_exit
    ~on_td_1_kong_2_exit
    ()
