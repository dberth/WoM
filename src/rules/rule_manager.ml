(*Copyright (C) 2014 Denis Berthod*)

type rule =
  {
    irregular_hands: Tileset.irregular_hands;
    evaluate_game: (Engine.player -> Engine.game -> float); 
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
  print_endline "REGISTER";
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

let current_rule = ref None

let all_rule_builders () =
  Hashtbl.fold (fun id _ acc -> id :: acc) rule_builders []

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

let set_rule rule_builder flags =
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
    current_rule := Some (build_rule test)
  | exception Not_found -> assert false
  end


let set_default_rule () =
  print_endline "ACCESS";
  match !default_rule_builder with
  | None -> assert false
  | Some rule_builder -> set_rule rule_builder.id None 

let irregular_hands () =
  match !current_rule with
  | None -> assert false
  | Some {irregular_hands; _} -> irregular_hands

let evaluate_game player game =
  match !current_rule with
  | None -> assert false
  | Some {evaluate_game; _} -> evaluate_game player game
