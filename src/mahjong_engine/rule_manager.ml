(*Copyright (C) 2014 Denis Berthod*)

type rule =
  {
    irregular_hands: Tileset.irregular_hands;
    seven_pairs: bool;
    evaluate_round: (Game_descr.round_player -> Engine.round -> float);
    explain_hand_score: (Engine.round -> (string * float) list * float);
    explain_player_score: (Game_descr.round_player -> Engine.round -> hand_score: float -> string * float);
  }

let new_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    !i

type flag_descr =
  {
    id: int;
    name: string;
  }

type flag = int

let flags = Hashtbl.create 4

let flag name =
  let id = new_id () in
  let flag = {id; name} in
  Hashtbl.add flags id flag;
  id

let name_of_flag flag =
  match Hashtbl.find flags flag with
  | {name; _} -> name
  | exception Not_found -> assert false

type rule_builer_descr =
  {
    id: int;
    name: string;
    flags: flag list;
    default_flags: flag list;
    build_rule: ((flag -> bool) -> rule);
  }

type rule_builder = int

let rule_builders = Hashtbl.create 4

let default_rule_builder = ref None

let register_rule_builder ~is_default ~flags ~default_flags ~build_rule name =
  let id = new_id () in
  let rule_builder =
    {
      id;
      name;
      flags;
      default_flags;
      build_rule;
    }
  in
  if is_default then default_rule_builder := Some rule_builder;
  Hashtbl.add rule_builders id rule_builder

let all_rule_builders () =
  Hashtbl.fold (fun id _ acc -> id :: acc) rule_builders []

let rule_builder_of_name s =
  Hashtbl.fold
    (fun id {name; _} result ->
       match result with
       | Some _ -> result
       | None ->
          if s = name then
            Some id
          else
            None
    )
    rule_builders
    None

let flags_of_flag_names rule_builder names =
  match names with
  | None -> None
  | Some names ->
    match Hashtbl.find rule_builders rule_builder with
    | {flags = builder_flags; _} ->
      Some
        (List.fold_left
           (fun acc flag ->
              match Hashtbl.find flags flag with
              | flag_descr ->
                if List.mem flag_descr.name names then
                  flag :: acc
                else
                  acc
              | exception Not_found -> assert false
           )
           []
           builder_flags
        )
    | exception Not_found -> assert false

  

let all_flags rule_builder =
  begin match Hashtbl.find rule_builders rule_builder with
  | {flags; _} -> flags
  | exception Not_found -> assert false
  end

let default_flags rule_builder =
  begin match Hashtbl.find rule_builders rule_builder with
  | {default_flags; _} -> default_flags
  | exception Not_found -> assert false
  end

let rule rule_builder flags =
  begin match Hashtbl.find rule_builders rule_builder with
  | {build_rule; default_flags; _} ->
    let flags =
      match flags with
      | None -> default_flags
      | Some flags -> flags
    in
    let max_flag = List.fold_left (fun acc x -> max acc x) 0 flags in
    let l = max_flag + 1 in
    let set = Array.make l false in
    List.iter (fun x -> set.(x) <- true) flags;
    let test flag =
      if max_flag < flag then
        false
      else
        set.(flag)
    in
    build_rule test
  | exception Not_found -> assert false
  end


let default_rule () =
  match !default_rule_builder with
  | None -> assert false
  | Some rule_builder -> rule rule_builder.id None 

let irregular_hands {irregular_hands; _} = irregular_hands

let seven_pairs {seven_pairs; _} = seven_pairs

let evaluate_round {evaluate_round; _} player round = evaluate_round player round

let explain_hand_score {explain_hand_score; _} round = explain_hand_score round

let explain_player_score {explain_player_score; _} player round ~hand_score =
  explain_player_score player round ~hand_score
