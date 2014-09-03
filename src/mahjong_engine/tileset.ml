(*Copyright (C) 2014 Denis Berthod*)

type tile_kind =
  | Char
  | Bam
  | Dot
  | Red_dragon
  | Green_dragon
  | White_dragon
  | East_wind
  | South_wind
  | North_wind
  | West_wind

type basic_tileset =
  | Num of (tile_kind * int array)
  | Honor of (tile_kind * int)

type tile = int

type tileset = basic_tileset list

type basic_tileset_trie = Trie of (basic_tileset * basic_tileset_trie) list

type irregular_hands =
  {
    with_isolated_tiles: basic_tileset_trie;
    without_isolated_tiles: basic_tileset_trie;
  }

type mahjong =
  | Regular of basic_tileset list
  | Irregular of tileset

type tile_descr =
  | Bam of int
  | Dot of int
  | Char of int
  | Red_dragon
  | Green_dragon
  | White_dragon
  | East_wind
  | South_wind
  | West_wind
  | North_wind

let compare_tiles x y = x - y

let empty = []

let default = Array.make 11 0

let num_op op x s =
  let s = Array.copy s in
  s.(x) <- op s.(x);
  s

let mk_position_chow start =
  let s = Array.copy default in
  s.(start) <- 1;
  s.(start + 1) <- 1;
  s.(start + 2) <- 1;
  s

let mk_position_pong index =
  let s = Array.copy default in
  s.(index) <- 3;
  s

let mk_position_kong index =
  let s = Array.copy default in
  s.(index) <- 4;
  s

let mk_position_pair index =
  let s = Array.copy default in
  s.(index) <- 2;
  s

let incr_num = num_op succ

let decr_num = num_op pred

let new_num x = incr_num x default

let ints_of_positions positions =
  let result = ref [] in
  for i = Array.length positions - 2 downto 1 do
    let t = positions.(i) in
    if t <> 0 then result := (i, t) :: !result
  done;
  !result

let rec make_list n v =
  if n = 0 then [] else v :: (make_list (n - 1) v)

let honor_tile_descr_of_kind : (tile_kind -> tile_descr) = function
  | Red_dragon -> Red_dragon
  | Green_dragon -> Green_dragon
  | White_dragon -> White_dragon
  | East_wind -> East_wind
  | South_wind -> South_wind
  | West_wind -> West_wind
  | North_wind -> North_wind
  | _ -> raise (Invalid_argument "honor_tile_descr_of_kind")

let num_tile_descr_of_kind num : (tile_kind -> tile_descr) = function
  | Char -> Char num
  | Bam -> Bam num
  | Dot -> Dot num
  | _ -> raise (Invalid_argument "num_tile_descr_of_kind")

let mk_honor_tile_descr kind nb =
  make_list nb (honor_tile_descr_of_kind kind)

let mk_num_tile_descr kind nums =
  List.flatten
    (List.map
       (fun (num, nb) -> make_list nb (num_tile_descr_of_kind num kind))
       nums
    )


let tile_descr_of_basic_tileset = function
  | Num (kind, positions) -> mk_num_tile_descr kind (ints_of_positions positions)
  | Honor (kind, nb) -> mk_honor_tile_descr kind nb

let tile_descr_of_tileset tileset =
  List.concat (List.map tile_descr_of_basic_tileset tileset)

let tile_descr_of_mahjong = function
  | Regular ts -> List.map tile_descr_of_basic_tileset ts
  | Irregular ts -> [List.map tile_descr_of_basic_tileset ts |> List.flatten]

let string_of_tile_descr = function
  | Bam i -> "b" ^ (string_of_int i)
  | Dot i -> "d" ^ (string_of_int i)
  | Char i -> "c" ^ (string_of_int i)
  | Red_dragon -> "rd"
  | Green_dragon -> "gd"
  | White_dragon -> "wd"
  | East_wind -> "ew"
  | South_wind -> "sw"
  | West_wind -> "ww"
  | North_wind -> "nw"

let compare_tile t1 t2 =
  match t1, t2 with
  | Num (kind1, _), Num (kind2, _)
  | Honor (kind1, _), Honor (kind2, _) -> compare kind1 kind2
  | _ -> compare t1 t2

let merge_positions s1 s2 =
  let s = Array.copy default in
  for i = 0 to 10 do
    s.(i) <- s1.(i) + s2.(i)
  done;
  s

let unmerge_positions s1 s2 =
  let s = Array.copy default in
  for i = 0 to 10 do
    s.(i) <- s1.(i) - s2.(i)
  done;
  s

let merge_tile t1 t2 =
  match t1, t2 with
  | Num(kind1, s1), Num(kind2, s2) ->
    assert (kind1 = kind2);
    Num(kind1, merge_positions s1 s2)
  | Honor(kind1, s1), Honor(kind2, s2) ->
    assert (kind1 = kind2);
    Honor(kind1, s1 + s2)
  | _ -> assert false

let unmerge_tile t1 t2 =
  match t1, t2 with
  | Num(kind1, s1), Num(kind2, s2) ->
    assert (kind1 = kind2);
    Num(kind1, unmerge_positions s1 s2)
  | Honor(kind1, s1), Honor(kind2, s2) ->
    assert (kind1 = kind2);
    Honor(kind1, s1 - s2)
  | _ -> assert false

let rec add_basic_tileset tile = function
  | [] -> [tile]
  | hd :: tl ->
    if compare_tile hd tile < 0 then
      hd :: (add_basic_tileset tile tl)
    else if compare_tile hd tile = 0 then
      (merge_tile hd tile) :: tl
    else
      tile :: hd :: tl

let for_all_positions f b =
  let result = ref true in
  let i = ref 0 in
  while !result = true && !i < Array.length b do
    result := f b.(!i);
    incr i
  done;
  !result

let is_empty_basic_tile_set = function
  | Num(_, s) -> for_all_positions (fun b -> b = 0) s
  | Honor (_, n) -> n = 0 

let rec remove_basic_tileset tile = function
  | [] -> raise Not_found
  | hd :: tl ->
    if compare_tile hd tile < 0 then
      hd :: (remove_basic_tileset tile tl)
    else if compare_tile hd tile = 0 then
      let new_basic_tileset = unmerge_tile hd tile in
      if is_empty_basic_tile_set new_basic_tileset then
        tl
      else
        new_basic_tileset :: tl
    else
      raise Not_found

let nb_tiles = 34

let tiles_rep = Array.make nb_tiles (Honor (Red_dragon, 0))

let tiles_descr = Array.make nb_tiles Red_dragon

let tile_descr_of_tile tile = tiles_descr.(tile)

let add_tile tile tileset = add_basic_tileset tiles_rep.(tile) tileset

let remove_tile tile tileset = remove_basic_tileset tiles_rep.(tile) tileset

let tileset_of_basic_tilesets l =
  List.fold_left (fun set tile -> add_basic_tileset tile set) [] l

let tileset_of_tiles l = tileset_of_basic_tilesets (List.map (fun i -> tiles_rep.(i)) l)

let basic_tileset_of_tiles l =
  match tileset_of_basic_tilesets l with
  | [x] -> x
  | _ -> raise (Invalid_argument "basic_tileset_of_tiles")

let new_tile =
  let i = ref (-1) in
  fun () -> incr i; !i

let new_char x =
  let tile = new_tile () in
  tiles_rep.(tile) <- Num (Char, new_num x);
  tiles_descr.(tile) <- Char x;
  tile

let c1 = new_char 1
let c2 = new_char 2
let c3 = new_char 3
let c4 = new_char 4
let c5 = new_char 5
let c6 = new_char 6
let c7 = new_char 7
let c8 = new_char 8
let c9 = new_char 9

let new_dot x =
  let tile = new_tile () in
  tiles_rep.(tile) <- Num (Dot, new_num x);
  tiles_descr.(tile) <- Dot x;
  tile

let d1 = new_dot 1
let d2 = new_dot 2
let d3 = new_dot 3
let d4 = new_dot 4
let d5 = new_dot 5
let d6 = new_dot 6
let d7 = new_dot 7
let d8 = new_dot 8
let d9 = new_dot 9

let new_bam x =
  let tile = new_tile () in
  tiles_rep.(tile) <- Num(Bam, new_num x);
  tiles_descr.(tile) <- Bam x;
  tile

let b1 = new_bam 1
let b2 = new_bam 2
let b3 = new_bam 3
let b4 = new_bam 4
let b5 = new_bam 5
let b6 = new_bam 6
let b7 = new_bam 7
let b8 = new_bam 8
let b9 = new_bam 9

let new_honor kind descr =
  let tile = new_tile () in
  tiles_rep.(tile) <- Honor(kind, 1);
  tiles_descr.(tile) <- descr;
  tile

let rd = new_honor Red_dragon Red_dragon
let wd = new_honor White_dragon White_dragon
let gd = new_honor Green_dragon Green_dragon

let ew = new_honor East_wind East_wind
let sw = new_honor South_wind South_wind
let ww = new_honor West_wind West_wind
let nw = new_honor North_wind North_wind

let rec read_010 index s =
  if 8 < index then
    false
  else
  if s.(index) = 0 then
    read_10 (index + 1) s
  else
    read_010 (index + 1) s

and read_10 index s =
  if 9 < index then
    false
  else
    match s.(index) with
    | 1 ->
      if s.(index + 1) = 0 then true else read_010 (index + 2) s
    | 0 -> read_10 (index + 1) s
    | _ -> read_010 (index + 1) s

(*very quick test that eleminate most of the tileset that aren't mahjong*)
let rec has_isolated_tile = function
  | [] -> false
  | Num (_, s) :: tl ->
    if read_010 0 s then true else has_isolated_tile tl
  | Honor (_, i) :: tl ->
    if i = 1 then true else has_isolated_tile tl

let rec read_1_chow start index s =
  if s.(start + index) <> 0 then
    if index = 2 then 1 else read_1_chow start (index + 1) s
  else
    0

and read_2_chows start index s =
  match s.(start + index) with
  | 0 -> 0
  | 1 ->
    if index = 2 then 1 else read_1_chow start (index + 1) s
  | _ ->
    if index = 2 then 2 else read_2_chows start (index + 1) s

and read_3_chows start index s =
  match s.(start + index) with
  | 0 -> 0
  | 1 ->
    if index = 2 then 1 else read_1_chow start (index + 1) s
  | 2 ->
    if index = 2 then 2 else read_2_chows start (index + 1) s
  | _ ->
    if index = 2 then 3 else read_3_chows start (index + 1) s

and read_4_chows start index s =
  match s.(start + index) with
  | 0 -> 0
  | 1 ->
    if index = 2 then 1 else read_1_chow start (index + 1) s
  | 2 ->
    if index = 2 then 2 else read_2_chows start (index + 1) s
  | 3 ->
    if index = 2 then 3 else read_3_chows start (index + 1) s
  | 4 ->
    if index = 2 then 4 else read_4_chows start (index + 1) s
  | _ -> assert false

let rec get_position_chows start s =
  if start = 8 then [] else
    let nb_chows = read_4_chows start 0 s in
    if nb_chows = 0 then
      get_position_chows (start + 1) s
    else
      (make_list nb_chows (mk_position_chow start)) @ (get_position_chows (start + 1) s)

let rec get_chows = function
  | [] -> []
  | Honor _ :: tl -> get_chows tl
  | Num (kind, s) :: tl ->
    let chows = List.map (fun s -> Num (kind, s)) (get_position_chows 1 s) in
    chows @ (get_chows tl)

let rec get_position_pong_and_kongs index s =
  if index = 10 then [] else
    match s.(index) with
    | 3 -> (mk_position_pong index) :: (get_position_pong_and_kongs (index + 1) s)
    | 4 -> (mk_position_kong index) :: (mk_position_pong index) :: (get_position_pong_and_kongs (index + 1) s)
    | _ -> get_position_pong_and_kongs (index + 1) s

let rec get_pong_and_kongs = function
  | [] -> []
  | Num (kind, s) :: tl ->
    let pong_and_kongs = List.map (fun s -> Num (kind, s)) (get_position_pong_and_kongs 1 s) in
    pong_and_kongs @ (get_pong_and_kongs tl)
  | Honor (kind, i) :: tl ->
    match i with
    | 3 -> Honor (kind, 3) :: (get_pong_and_kongs tl)
    | 4 -> Honor (kind, 4) :: Honor (kind, 3) :: (get_pong_and_kongs tl)
    | _ -> get_pong_and_kongs tl

let rec get_position_pairs index s =
  if index = 10 then [] else
    match s.(index) with
    | 2 | 3 -> (mk_position_pair index) :: (get_position_pairs (index + 1) s)
    | 4 -> let p = mk_position_pair index in p :: p :: (get_position_pairs (index + 1) s)
    | _ -> get_position_pairs (index +1) s

let rec get_pairs = function
  | [] -> []
  | Num(kind, s) :: tl ->
    let pairs = List.map (fun s -> Num (kind, s)) (get_position_pairs 1 s) in
    pairs @ (get_pairs tl)
  | Honor(kind, i) :: tl ->
    let p = Honor(kind, 2) in
    match i with
    | 2 | 3 -> p :: (get_pairs tl)
    | 4 -> p :: p :: (get_pairs tl)
    | _ -> get_pairs tl

let get_3sets tileset =
  get_chows tileset @ get_pong_and_kongs tileset


let rec add_tileset_to_trie ts trie =
  match ts with
  | [] -> trie
  | bt :: ts_tail ->
    match trie with
    | Trie [] -> Trie [bt, add_tileset_to_trie ts_tail (Trie [])]
    | Trie ((hd_bt, hd_trie) :: tl) when bt = hd_bt -> Trie ((hd_bt, add_tileset_to_trie ts_tail hd_trie) :: tl)
    | Trie (hd :: tl) ->
      let Trie trie = add_tileset_to_trie ts (Trie tl) in
      Trie (hd :: trie)

let rec tileset_in_trie ts trie =
  match ts, trie with
  | [], Trie [] -> true
  | _, Trie [] | [], _ -> false
  | bt :: ts_tail, Trie ((hd_bt, hd_trie) ::  _) when bt = hd_bt -> tileset_in_trie ts_tail hd_trie
  | _, Trie (_ :: tl) -> tileset_in_trie ts (Trie tl)

let no_irregular_hand = {with_isolated_tiles = Trie []; without_isolated_tiles = Trie []}

let add_irregular_hand tiles {with_isolated_tiles; without_isolated_tiles} =
  let tileset = tileset_of_tiles tiles in
  let with_isolated_tiles, without_isolated_tiles =
    if has_isolated_tile tileset then
      add_tileset_to_trie tileset without_isolated_tiles, without_isolated_tiles
    else
      with_isolated_tiles, add_tileset_to_trie tileset without_isolated_tiles
  in
  {with_isolated_tiles; without_isolated_tiles}

let rev_flatten l =
  List.fold_left (fun acc l -> List.rev_append l acc) [] l

let bind l f =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (f hd tl :: acc) tl
  in
  rev_flatten (aux [] l)

let (>>=) = bind

let rec mahjong_aux sets result nb_3_sets hand =
  if nb_3_sets = 0 then
    get_pairs hand >>= fun pair _ ->
    [pair :: result]
  else
    sets >>= fun set rest ->
    mahjong_aux rest (set :: result) (nb_3_sets - 1) hand

let mahjong nb_3sets hand =
  if has_isolated_tile hand then begin
    []
  end else begin
    let candidates = mahjong_aux (get_3sets hand) [] nb_3sets hand in
    List.filter (fun tiles -> try tileset_of_basic_tilesets tiles = hand with Invalid_argument _ -> false) candidates
  end


let rev_flatten l =
  List.fold_left (fun acc l -> List.rev_append l acc) [] l

let bind l f =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (f hd tl :: acc) tl
  in
  rev_flatten (aux [] l)

let (>>=) = bind

let rec mahjong_aux sets result nb_3_sets hand =
  if nb_3_sets = 0 then
    get_pairs hand >>= fun pair _ ->
    [pair :: result]
  else
    sets >>= fun set rest ->
    mahjong_aux rest (set :: result) (nb_3_sets - 1) hand

let no_irregular_hands = {with_isolated_tiles = Trie []; without_isolated_tiles = Trie []}

let real_regular_mahjong_check nb_3sets hand =
  let candidates = mahjong_aux (get_3sets hand) [] nb_3sets hand in
  let results =
    List.filter (fun tiles -> try tileset_of_basic_tilesets tiles = hand with Invalid_argument _ -> false) candidates
  in
  List.map (fun x -> Regular x) results

let real_mahjong_check irregular_hands nb_3sets hand =
  if nb_3sets = 4 then
    if tileset_in_trie hand irregular_hands then
      [Irregular hand]
    else
      real_regular_mahjong_check nb_3sets hand
  else
    real_regular_mahjong_check nb_3sets hand


let mahjong ?(irregular_hands = no_irregular_hands) nb_3sets hand =
  if has_isolated_tile hand then begin
    if nb_3sets = 4 then
      if tileset_in_trie hand irregular_hands.with_isolated_tiles then
        [Irregular hand]
      else
        []
    else
      []
  end else
    real_mahjong_check irregular_hands.without_isolated_tiles nb_3sets hand

let is_kong_position b =
  let has_wrong_tile = ref false in
  let has_kong = ref false in
  for i = 0 to Array.length b - 1 do
    match b.(i) with
    | 0 -> ()
    | 1 | 2 | 3 -> has_wrong_tile := true
    | 4 ->
      if !has_kong then
        has_wrong_tile := true
      else
        has_kong := true
    | _ -> assert false
  done;
  not !has_wrong_tile && !has_kong

let is_pong_position b =
  let has_wrong_tile = ref false in
  let has_pong = ref false in
  for i = 0 to Array.length b - 1 do
    match b.(i) with
    | 0 -> ()
    | 1 | 2 | 4 -> has_wrong_tile := true
    | 3 ->
      if !has_pong then
        has_wrong_tile := true
      else
        has_pong := true
    | _ -> assert false
  done;
  not !has_wrong_tile && !has_pong

let is_kong = function
  | [Honor(_, 4)] -> true
  | [Num(_, position)] -> is_kong_position position
  | _ -> false

let get_kongs tileset =
  List.fold_left
    (fun acc basic_tileset ->
      if is_kong [basic_tileset] then
        List.hd (tile_descr_of_basic_tileset basic_tileset) :: acc
      else
        acc
    )
    []
    tileset

let is_pong = function
  | [Honor (_, 3)] -> true
  | [Num(_, position)] -> is_pong_position position
  | _ -> false

let tiles_to_chow build i =
  let result =
    if 2 < i then
      [[build (i -2); build (i - 1)]]
    else
      []
  in
  let result =
    if 1 < i && i < 9 then
      [build (i -1); build (i + 1)] :: result
    else
      result
  in
  if i < 8 then
    [build (i + 1); build (i + 2)] :: result
  else
    result

let tiles_to_chow = function
  | Bam i -> tiles_to_chow (fun i -> Bam i) i
  | Dot i -> tiles_to_chow (fun i -> Dot i) i
  | Char i -> tiles_to_chow (fun i -> Char i) i
  | _ -> [] 

type tileset_status =
  {
    alone: tile_descr list;
    in_sub_chow: tile_descr list;
    in_pair: tile_descr list;
    in_3set: tile_descr list;
  }

let status_of_honor_set alone set2 set3 kind x =
  let tile = honor_tile_descr_of_kind kind in
  match x with
  | 0 -> alone, set2, set3
  | 1 -> tile :: alone, set2, set3
  | 2 -> alone, tile :: set2, set3
  | 3 | 4 -> alone, set2, tile :: set3
  | _ -> assert false

let set_series position status_array =
  let set i status =
    status_array.(i - 1) <- max status_array.(i - 1) status
  in
  for i = 1 to 7 do
    match position.(i) <> 0, position.(i + 1) <> 0, position.(i + 2) <> 0 with
    | false, false, false -> ()
    | true, false, false -> set i 1
    | false, true, false -> set (i + 1) 1
    | true, true, false -> set i 2; set (i + 1) 2
    | false, false, true -> set (i + 2) 1
    | true, false, true -> set i 2; set (i + 2) 2
    | false, true, true -> set (i + 1) 2; set (i + 2) 2
    | true, true, true -> set i 4; set (i + 1) 4; set (i + 2) 4
  done

let set_groups position status_array =
  let set i status =
    status_array.(i - 1) <- max status_array.(i - 1) status
  in
  for i = 1 to 9 do
    match position.(i) with
    | 0 -> ()
    | 1 -> set i 1
    | 2 -> set i 3
    | 3 | 4 -> set i 4
    | _ -> assert false
  done

let status_of_num_set alone sub_chow set2 set3 kind position =
  let status_array = Array.make 9 0 in
  set_series position status_array;
  set_groups position status_array;
  let alone = ref alone in
  let sub_chow = ref sub_chow in
  let set2 = ref set2 in
  let set3 = ref set3 in
  Array.iteri
    (fun i status -> 
      match status with
      | 0 -> ()
      | 1 -> alone := num_tile_descr_of_kind (i + 1) kind :: !alone
      | 2 -> sub_chow := num_tile_descr_of_kind (i + 1) kind :: !sub_chow
      | 3 -> set2 := num_tile_descr_of_kind (i + 1) kind :: !set2
      | 4 -> set3 := num_tile_descr_of_kind (i + 1) kind :: !set3
      | _ -> assert false
    )
    status_array;
  !alone, !sub_chow, !set2, !set3

let status_of_tileset tileset =
  let rec aux alone sub_chow set2 set3 = function
    | [] -> {alone; in_sub_chow = sub_chow; in_pair = set2; in_3set = set3}
    | Honor (kind, x) :: tl ->
      let alone, set2, set3 = status_of_honor_set alone set2 set3 kind x in
      aux alone sub_chow set2 set3 tl
    | Num (kind, position) :: tl ->
      let alone, sub_chow, set2, set3 = status_of_num_set alone sub_chow set2 set3 kind position in
      aux alone set2 sub_chow set3 tl
  in
  aux [] [] [] [] tileset

type tile_multi_set =
  | Empty
  | Node of (tile_multi_set * tile * int * tile_multi_set)

let empty_multi_set = Empty

let rec multi_set_cardinal = function
  | Empty -> 0
  | Node (right, _, nb, left) -> multi_set_cardinal right + nb + multi_set_cardinal left

let rec add_tile_in_multi_set tile = function
  | Empty -> Node (Empty, tile, 1, Empty)
  | Node (left, t, nb, right) ->
    if t = tile then
      Node(left, t, nb + 1, right)
    else if tile < t then
      Node (add_tile_in_multi_set tile left, t, nb, right)
    else
      Node (left, t, nb, add_tile_in_multi_set tile right)

let rec remove_tile_from_multi_set tile = function
  | Empty -> Empty
  | Node (left, t, nb, right) ->
    if t = tile then
      Node (left, t, max (nb - 1)  0, right)
    else if tile < t then
      Node (remove_tile_from_multi_set tile left, t, nb, right)
    else
      Node (left, t, nb, remove_tile_from_multi_set tile right)
      

let rec iter_multi_set f = function
  | Empty -> ()
  | Node (left, t, nb, right) ->
    if nb <> 0 then for i = 1 to nb do f t done;
    iter_multi_set f left;
    iter_multi_set f right
