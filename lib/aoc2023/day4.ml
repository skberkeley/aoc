type card = int list * int list

let card_re = Str.regexp {|Card +[0-9]+:\([ 0-9]+\)|\([ 0-9]+\)|}

module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)

let int_list_of_string : string -> int list = function
  | s ->
      String.trim s |> String.split_on_char ' '
      |> List.filter (function s -> s <> "")
      |> List.map int_of_string

let card_of_string : string -> card =
 fun s ->
  if Str.string_match card_re s 0 then
    ( int_list_of_string (Str.matched_group 1 s)
    , int_list_of_string (Str.matched_group 2 s) )
  else failwith "Card regex did not match"

let rec num_in_set : Int_set.t -> int list -> int =
 fun s lst ->
  match lst with
  | [] ->
      0
  | n :: lst ->
      if Int_set.mem n s then 1 + num_in_set s lst else num_in_set s lst

let rec power_of_two : int -> int =
 fun n -> match n with 0 -> 0 | 1 -> 1 | n -> 2 * power_of_two (n - 1)

let num_winning_numbers : card -> int = function
  | winning, my_nums ->
      let winning_num_set = Int_set.of_list winning in
      num_in_set winning_num_set my_nums

let score_card : card -> int = function
  | c ->
      num_winning_numbers c |> power_of_two

let solve_part1 : string list -> string =
 fun lst ->
  List.map card_of_string lst
  |> List.map score_card |> List.fold_left ( + ) 0 |> string_of_int

let rec range : int -> int -> int list =
 fun i j -> if i > j then [] else if i = j then [i] else i :: range (i + 1) j

let rec calculate_num_cards : int Int_map.t -> int -> int -> card list -> int =
 fun num_card_copies total i cards ->
  Printf.printf "calculate_num_cards: i: %d\n" i ;
  match cards with
  | [] ->
      total
  | c :: lst ->
      let num_winning = num_winning_numbers c in
      Printf.printf "calculate_num_cards: num_winning: %d\n" num_winning ;
      let copies_won = range (i + 1) (i + num_winning) in
      Printf.printf "calculate_num_cards: copies_won: %s\n"
        (List.map string_of_int copies_won |> String.concat " ") ;
      let updated_num_copies =
        List.map
          (fun n ->
            (n, Int_map.find n num_card_copies + Int_map.find i num_card_copies)
            )
          copies_won
        |> List.to_seq
      in
      let num_card_copies =
        Int_map.add_seq updated_num_copies num_card_copies
      in
      calculate_num_cards num_card_copies
        (total + Int_map.find i num_card_copies)
        (i + 1) lst

let solve_part2 : string list -> string =
 fun lst ->
  let starting_counts =
    range 0 (List.length lst - 1)
    |> List.map (fun i -> (i, 1))
    |> List.to_seq |> Int_map.of_seq
  in
  List.map card_of_string lst
  |> calculate_num_cards starting_counts 0 0
  |> string_of_int

let solve : string list -> string = fun lst -> solve_part2 lst
