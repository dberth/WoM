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
  let s = String.copy default in
  for i = 0 to 10 do
    Bytes.set s i (add_byte (Bytes.get s1 i) (Bytes.get s2 i))
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

let rec add_basic_tileset tile = function
  | [] -> [tile]
  | hd :: tl ->
      if compare_tile hd tile < 0 then
        hd :: (add_basic_tileset tile tl)
      else if compare_tile hd tile = 0 then
        (merge_tile hd tile) :: tl
      else
        tile :: hd :: tl

let add_tile = add_basic_tileset

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
