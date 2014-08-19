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
  | Num of (tile_kind * Bytes.t)
  | Honor of (tile_kind * int)

type tile = basic_tileset

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

let empty = []

let default = String.make 11 'z'

let incr_byte = function
  | 'z' -> 'a'
  | 'a' -> 'b'
  | 'b' -> 'c'
  | 'c' -> 'd'
  | _ -> raise (Invalid_argument "incr_byte")

let decr_byte = function
  | 'a' -> 'z'
  | 'b' -> 'a'
  | 'c' -> 'b'
  | 'd' -> 'c'
  | _ -> raise (Invalid_argument "decr_byte")

let add_byte b1 b2 =
  match b1, b2 with
  | 'z', b | b, 'z' -> b
  | 'a', b | b, 'a' -> incr_byte b
  | 'b', b | b, 'b' -> incr_byte (incr_byte b)
  | _ -> raise (Invalid_argument "add_byte")

let sub_byte b1 b2 =
  match b1, b2 with
  | _, 'z' -> b1
  | _, 'a' -> decr_byte b1
  | _, 'b' -> decr_byte (decr_byte b1)
  | 'c', 'c' -> 'z'
  | 'd', 'c' -> 'a'
  | 'd', 'd' -> 'z'
  | _ -> raise (Invalid_argument "sub_byte")


let num_op op x s =
  let s = Bytes.copy s in
  Bytes.set s x (op (Bytes.get s x));
  s

let mk_byte_show start =
  let s = Bytes.copy default in
  Bytes.set s start 'a';
  Bytes.set s (start + 1) 'a';
  Bytes.set s (start + 2) 'a';
  s

let mk_byte_pong index =
  let s = Bytes.copy default in
  Bytes.set s index 'c';
  s

let mk_byte_kong index =
  let s = Bytes.copy default in
  Bytes.set s index 'd';
  s

let mk_byte_pair index =
  let s = Bytes.copy default in
  Bytes.set s index 'b';
  s

let incr_num = num_op incr_byte

let decr_num = num_op decr_byte

let new_num x = incr_num x default

let int_of_byte = function
  | 'z' -> 0
  | 'a' -> 1
  | 'b' -> 2
  | 'c' -> 3
  | 'd' -> 4
  | _ -> raise (Invalid_argument "int_of_byte")

let ints_of_bytes b =
  let result = ref [] in
  for i = Bytes.length b - 2 downto 1 do
    let t = int_of_byte (Bytes.get b i) in
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
  | Num (kind, bytes) -> mk_num_tile_descr kind (ints_of_bytes bytes)
  | Honor (kind, nb) -> mk_honor_tile_descr kind nb

let tile_descr_of_tile tile =
  match tile_descr_of_basic_tileset tile with
  | [x] -> x
  | _ -> assert false

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

let merge_byte s1 s2 =
  let s = Bytes.copy default in
  for i = 0 to 10 do
    Bytes.set s i (add_byte (Bytes.get s1 i) (Bytes.get s2 i))
  done;
  s

let unmerge_byte s1 s2 =
  let s = Bytes.copy default in
  for i = 0 to 10 do
    Bytes.set s i (sub_byte (Bytes.get s1 i) (Bytes.get s2 i))
  done;
  s

let merge_tile t1 t2 =
  match t1, t2 with
  | Num(kind1, s1), Num(kind2, s2) ->
    assert (kind1 = kind2);
    Num(kind1, merge_byte s1 s2)
  | Honor(kind1, s1), Honor(kind2, s2) ->
    assert (kind1 = kind2);
    Honor(kind1, s1 + s2)
  | _ -> assert false

let unmerge_tile t1 t2 =
  match t1, t2 with
  | Num(kind1, s1), Num(kind2, s2) ->
    assert (kind1 = kind2);
    Num(kind1, unmerge_byte s1 s2)
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

let for_all_bytes f b =
  let result = ref true in
  let i = ref 0 in
  while !result = true && !i < Bytes.length b do
    result := f (Bytes.get b !i);
    incr i
  done;
  !result

let is_empty_basic_tile_set = function
  | Num(_, s) -> for_all_bytes (fun b -> b = 'z') s
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

let add_tile = add_basic_tileset

let remove_tile = remove_basic_tileset

let tileset_of_basic_tilesets l =
  List.fold_left (fun set tile -> add_basic_tileset tile set) [] l

let tileset_of_tiles l = tileset_of_basic_tilesets l

let basic_tileset_of_tiles l =
  match tileset_of_basic_tilesets l with
  | [x] -> x
  | _ -> raise (Invalid_argument "basic_tileset_of_tiles")

let new_char x = Num (Char, new_num x)

let c1 = new_char 1
let c2 = new_char 2
let c3 = new_char 3
let c4 = new_char 4
let c5 = new_char 5
let c6 = new_char 6
let c7 = new_char 7
let c8 = new_char 8
let c9 = new_char 9

let new_dot x = Num (Dot, new_num x)

let d1 = new_dot 1
let d2 = new_dot 2
let d3 = new_dot 3
let d4 = new_dot 4
let d5 = new_dot 5
let d6 = new_dot 6
let d7 = new_dot 7
let d8 = new_dot 8
let d9 = new_dot 9

let new_bam x = Num(Bam, new_num x)

let b1 = new_bam 1
let b2 = new_bam 2
let b3 = new_bam 3
let b4 = new_bam 4
let b5 = new_bam 5
let b6 = new_bam 6
let b7 = new_bam 7
let b8 = new_bam 8
let b9 = new_bam 9

let rd = Honor (Red_dragon, 1)
let wd = Honor (White_dragon, 1)
let gd = Honor (Green_dragon, 1)

let ew = Honor (East_wind, 1)
let sw = Honor (South_wind, 1)
let ww = Honor (West_wind, 1)
let nw = Honor (North_wind, 1)

let rec read_zaz index s =
  if 8 < index then
    false
  else
  if s.[index] = 'z' then
    read_az (index + 1) s
  else
    read_zaz (index + 1) s

and read_az index s =
  if 9 < index then
    false
  else
    match s.[index] with
    | 'a' ->
      if s.[index + 1] = 'z' then true else read_zaz (index + 2) s
    | 'z' -> read_az (index + 1) s
    | _ -> read_zaz (index + 1) s

(*very quick test that eleminate most of the tileset that aren't mahjong*)
let rec has_isolated_tile = function
  | [] -> false
  | Num (_, s) :: tl ->
    if read_zaz 0 s then true else has_isolated_tile tl
  | Honor (_, i) :: tl ->
    if i = 1 then true else has_isolated_tile tl

let rec read_1_show start index s =
  if s.[start + index] <> 'z' then
    if index = 2 then 1 else read_1_show start (index + 1) s
  else
    0

and read_2_shows start index s =
  match s.[start + index] with
  | 'z' -> 0
  | 'a' ->
    if index = 2 then 1 else read_1_show start (index + 1) s
  | _ ->
    if index = 2 then 2 else read_2_shows start (index + 1) s

and read_3_shows start index s =
  match s.[start + index] with
  | 'z' -> 0
  | 'a' ->
    if index = 2 then 1 else read_1_show start (index + 1) s
  | 'b' ->
    if index = 2 then 2 else read_2_shows start (index + 1) s
  | _ ->
    if index = 2 then 3 else read_3_shows start (index + 1) s

and read_4_shows start index s =
  match s.[start + index] with
  | 'z' -> 0
  | 'a' ->
    if index = 2 then 1 else read_1_show start (index + 1) s
  | 'b' ->
    if index = 2 then 2 else read_2_shows start (index + 1) s
  | 'c' ->
    if index = 2 then 3 else read_3_shows start (index + 1) s
  | 'd' ->
    if index = 2 then 4 else read_4_shows start (index + 1) s
  | _ -> assert false

let rec get_byte_shows start s =
  if start = 8 then [] else
    let nb_shows = read_4_shows start 0 s in
    if nb_shows = 0 then
      get_byte_shows (start + 1) s
    else
      (make_list nb_shows (mk_byte_show start)) @ (get_byte_shows (start + 1) s)

let rec get_shows = function
  | [] -> []
  | Honor _ :: tl -> get_shows tl
  | Num (kind, s) :: tl ->
    let shows = List.map (fun s -> Num (kind, s)) (get_byte_shows 1 s) in
    shows @ (get_shows tl)

let rec get_byte_pong_and_kongs index s =
  if index = 10 then [] else
    match s.[index] with
    | 'c' -> (mk_byte_pong index) :: (get_byte_pong_and_kongs (index + 1) s)
    | 'd' -> (mk_byte_kong index) :: (mk_byte_pong index) :: (get_byte_pong_and_kongs (index + 1) s)
    | _ -> get_byte_pong_and_kongs (index + 1) s

let rec get_pong_and_kongs = function
  | [] -> []
  | Num (kind, s) :: tl ->
    let pong_and_kongs = List.map (fun s -> Num (kind, s)) (get_byte_pong_and_kongs 1 s) in
    pong_and_kongs @ (get_pong_and_kongs tl)
  | Honor (kind, i) :: tl ->
    match i with
    | 3 -> Honor (kind, 3) :: (get_pong_and_kongs tl)
    | 4 -> Honor (kind, 4) :: Honor (kind, 3) :: (get_pong_and_kongs tl)
    | _ -> get_pong_and_kongs tl

let rec get_byte_pairs index s =
  if index = 10 then [] else
    match s.[index] with
    | 'b' | 'c' -> (mk_byte_pair index) :: (get_byte_pairs (index + 1) s)
    | 'd' -> let p = mk_byte_pair index in p :: p :: (get_byte_pairs (index + 1) s)
    | _ -> get_byte_pairs (index +1) s

let rec get_pairs = function
  | [] -> []
  | Num(kind, s) :: tl ->
    let pairs = List.map (fun s -> Num (kind, s)) (get_byte_pairs 1 s) in
    pairs @ (get_pairs tl)
  | Honor(kind, i) :: tl ->
    let p = Honor(kind, 2) in
    match i with
    | 2 | 3 -> p :: (get_pairs tl)
    | 4 -> p :: p :: (get_pairs tl)
    | _ -> get_pairs tl

let get_3sets tileset =
  get_shows tileset @ get_pong_and_kongs tileset


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

let is_kong_bytes b =
  let has_wrong_tile = ref false in
  let has_kong = ref false in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'z' -> ()
    | 'a'..'c' -> has_wrong_tile := true
    | 'd' ->
      if !has_kong then
        has_wrong_tile := true
      else
        has_kong := true
    | _ -> assert false
  done;
  not !has_wrong_tile && !has_kong

let is_pong_bytes b =
  let has_wrong_tile = ref false in
  let has_pong = ref false in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'z' -> ()
    | 'a' | 'b' | 'd' -> has_wrong_tile := true
    | 'c' ->
      if !has_pong then
        has_wrong_tile := true
      else
        has_pong := true
    | _ -> assert false
  done;
  not !has_wrong_tile && !has_pong

let is_kong = function
  | [Honor(_, 4)] -> true
  | [Num(_, bytes)] -> is_kong_bytes bytes
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
  | [Num(_, bytes)] -> is_pong_bytes bytes
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

let set_series bytes status_array =
  let set i status =
    status_array.(i - 1) <- max status_array.(i - 1) status
  in
  for i = 1 to 7 do
    match Bytes.get bytes i <> 'z', Bytes.get bytes (i + 1) <> 'z', Bytes.get bytes (i + 2) <> 'z' with
    | false, false, false -> ()
    | true, false, false -> set i 1
    | false, true, false -> set (i + 1) 1
    | true, true, false -> set i 2; set (i + 1) 2
    | false, false, true -> set (i + 2) 1
    | true, false, true -> set i 2; set (i + 2) 2
    | false, true, true -> set (i + 1) 2; set (i + 2) 2
    | true, true, true -> set i 4; set (i + 1) 4; set (i + 2) 4
  done

let set_groups bytes status_array =
  let set i status =
    status_array.(i - 1) <- max status_array.(i - 1) status
  in
  for i = 1 to 9 do
    match Bytes.get bytes i with
    | 'z' -> ()
    | 'a' -> set i 1
    | 'b' -> set i 3
    | 'c' | 'd' -> set i 4
    | _ -> assert false
  done

let status_of_num_set alone sub_chow set2 set3 kind bytes =
  let status_array = Array.make 9 0 in
  set_series bytes status_array;
  set_groups bytes status_array;
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
    | Num (kind, bytes) :: tl ->
      let alone, sub_chow, set2, set3 = status_of_num_set alone sub_chow set2 set3 kind bytes in
      aux alone set2 sub_chow set3 tl
  in
  aux [] [] [] [] tileset
