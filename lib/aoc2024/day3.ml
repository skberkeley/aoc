type instr = Mul of int * int | Do | Dont

let string_of_instr = function
  | Mul (n1, n2) ->
      Printf.sprintf "Mul(%d, %d)" n1 n2
  | Do ->
      "Do"
  | Dont ->
      "Don't"

type input_type = instr list

let mul_re =
  Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))\|do()\|don't()|}

let rec find_instrs : string -> input_type =
 fun s ->
  try
    let _ = Str.search_forward mul_re s 0 in
    let instr =
      match Str.matched_string s with
      | "do()" ->
          Do
      | "don't()" ->
          Dont
      | _ ->
          let num1 = Str.matched_group 1 s |> int_of_string in
          let num2 = Str.matched_group 2 s |> int_of_string in
          Mul (num1, num2)
    in
    let s_index = Str.match_end () in
    let s' = String.sub s s_index (String.length s - s_index) in
    instr :: find_instrs s'
  with Not_found -> []

let parse : string list -> input_type =
 fun lst -> List.map find_instrs lst |> List.flatten

let solve_part_1 : input_type -> string =
 fun lst ->
  let () = List.iter (fun i -> Printf.printf "%s\n" (string_of_instr i)) lst in
  List.map (fun i -> match i with Mul (n, n') -> n * n' | Do | Dont -> 0) lst
  |> List.fold_left ( + ) 0 |> string_of_int

let solve_part_2 : input_type -> string =
 fun lst ->
  let rec helper (lst : input_type) (muls_enabled : bool) (sum : int) =
    match lst with
    | [] ->
        sum
    | Do :: lst ->
        helper lst true sum
    | Dont :: lst ->
        helper lst false sum
    | Mul (n1, n2) :: lst ->
        let sum = if muls_enabled then sum + (n1 * n2) else sum in
        helper lst muls_enabled sum
  in
  helper lst true 0 |> string_of_int
