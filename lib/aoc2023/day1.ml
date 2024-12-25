let get_num : string -> int =
 fun s ->
  let re1 = Str.regexp {|^[a-z]*\([0-9]\)[a-z0-9]*\([0-9]\)[a-z]*$|} in
  let re2 = Str.regexp {|^[a-z]*\([0-9]\)[a-z]*$|} in
  if Str.string_match re1 s 0 then
    let a = Str.matched_group 1 s |> int_of_string in
    let b = Str.matched_group 2 s |> int_of_string in
    (a * 10) + b
  else if Str.string_match re2 s 0 then
    let a = Str.matched_group 1 s |> int_of_string in
    a * 11
  else failwith (Printf.sprintf "invalid input: %s" s)

let int_of_word : string -> int =
 fun s ->
  match s with
  | "one" ->
      1
  | "two" ->
      2
  | "three" ->
      3
  | "four" ->
      4
  | "five" ->
      5
  | "six" ->
      6
  | "seven" ->
      7
  | "eight" ->
      8
  | "nine" ->
      9
  | _ ->
      int_of_string s

let rev_num : string -> int =
 fun s ->
  match s with
  | "eno" ->
      1
  | "owt" ->
      2
  | "eerht" ->
      3
  | "ruof" ->
      4
  | "evif" ->
      5
  | "xis" ->
      6
  | "neves" ->
      7
  | "thgie" ->
      8
  | "enin" ->
      9
  | _ ->
      int_of_string s

let get_num_p2 : string -> int =
 fun s ->
  let forwards =
    Str.regexp {|one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|[0-9]|}
  in
  (* let backwards = Str.regexp {|eno\|owt\|eerht\|ruof\|evif\|xis\|neves\|thgie\|enin\|[0-9]|} in *)
  let _ = Str.search_forward forwards s 0 in
  let a = Str.matched_string s |> int_of_word in
  print_endline (string_of_int a) ;
  let _ = Str.search_backward forwards s (String.length s - 1) in
  let b = Str.matched_string s |> int_of_word in
  print_endline (string_of_int b) ;
  (a * 10) + b

let solve : string list -> string =
 fun lines ->
  List.map get_num_p2 lines |> List.fold_left ( + ) 0 |> string_of_int

let solve_part_1 : string list -> string = fun lines -> solve lines

let solve_part_2 : string list -> string = fun lines -> solve lines
