type input_type = (int * int) list

let mul_re = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

let rec find_muls : string -> input_type =
 fun s ->
  try
    let _ = Str.search_forward mul_re s 0 in
    let num1 = Str.matched_group 1 s |> int_of_string in
    let num2 = Str.matched_group 2 s |> int_of_string in
    let s_index = Str.match_end () in
    let s' = String.sub s s_index (String.length s - s_index) in
    (num1, num2) :: find_muls s'
  with Not_found -> []

let parse : string list -> input_type =
 fun lst -> List.map find_muls lst |> List.flatten

let solve_part_1 : input_type -> string =
 fun lst ->
  List.map (fun (n1, n2) -> n1 * n2) lst
  |> List.fold_left ( + ) 0 |> string_of_int

let solve_part_2 : input_type list -> string = fun _ -> failwith "todo"
