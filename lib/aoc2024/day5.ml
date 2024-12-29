type input_type = (int * int) list * int list list

let rec parse_rules (lst : string list) =
  let rule_re = Str.regexp {|\([0-9]+\)|\([0-9]+\)|} in
  match lst with
  | "" :: lst ->
      ([], lst)
  | s :: lst ->
      let _ = Str.string_match rule_re s 0 in
      let num1, num2 = (Str.matched_group 1 s, Str.matched_group 2 s) in
      let rules, lst = parse_rules lst in
      ((int_of_string num1, int_of_string num2) :: rules, lst)
  | _ ->
      failwith "Unexpected input"

let parse : string list -> input_type =
 fun lst ->
  let rules, lst = parse_rules lst in
  let updates =
    List.map (Str.split (Str.regexp {|,|})) lst
    |> List.map (List.map int_of_string)
  in
  (rules, updates)

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let map_of_rules (rules : (int * int) list) =
  List.fold_left
    (fun map rule ->
      match rule with
      | n1, n2 ->
          if IntMap.mem n1 map then
            IntMap.add n1 (IntSet.add n2 (IntMap.find n1 map)) map
          else IntMap.add n1 (IntSet.singleton n2) map )
    IntMap.empty rules

let update_ok (rule_map : IntSet.t IntMap.t) (update : int list) =
  let rec helper (seen : IntSet.t) (update : int list) =
    match update with
    | [] ->
        true
    | n :: update ->
        if IntMap.mem n rule_map then
          let intersection = IntSet.inter (IntMap.find n rule_map) seen in
          if not (IntSet.is_empty intersection) then false
          else
            let seen = IntSet.add n seen in
            helper seen update
        else
          let seen = IntSet.add n seen in
          helper seen update
  in
  helper IntSet.empty update

let get_middle (lst : int list) =
  try List.nth lst (List.length lst / 2)
  with Failure _ as e ->
    let () = print_endline (Util.string_of_int_list lst) in
    raise e

let solve_part_1 : input_type -> string = function
  | rules, updates ->
      let rule_map = map_of_rules rules in
      let ok_updates = List.filter (update_ok rule_map) updates in
      List.map get_middle ok_updates |> List.fold_left ( + ) 0 |> string_of_int

let fix_update (rule_map : IntSet.t IntMap.t) (update : int list) =
  List.stable_sort
    (fun a b ->
      if IntMap.mem b rule_map && IntSet.mem a (IntMap.find b rule_map) then 1
      else if IntMap.mem a rule_map && IntSet.mem b (IntMap.find a rule_map)
      then -1
      else 0 )
    update

let solve_part_2 : input_type -> string = function
  | rules, updates ->
      let rule_map = map_of_rules rules in
      let bad_updates =
        List.filter (fun u -> not (update_ok rule_map u)) updates
      in
      let fixed_updates = List.map (fix_update rule_map) bad_updates in
      List.map get_middle fixed_updates |> Util.sum_list |> string_of_int
