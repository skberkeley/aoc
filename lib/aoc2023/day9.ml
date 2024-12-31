type reading = int array

let parse_reading : string -> reading = function
  | s ->
      String.split_on_char ' ' s |> List.map int_of_string |> Array.of_list

let parse_values : string list -> reading list = function
  | lst ->
      List.map parse_reading lst

let n_choose_k : int -> int -> int =
 fun n k ->
  let rec calc_range lo hi acc =
    if lo > hi then acc else calc_range (lo + 1) hi (acc * lo)
  in
  let c =
    if k > n - k then calc_range (k + 1) n 1 / Util.factorial (n - k)
    else calc_range (n - k + 1) n 1 / Util.factorial k
  in
  (* Printf.printf "n_choose_k: n: %d, k : %d, c: %d\n" n k c; *)
  c

(* compute and evaluate the ith term of the lagrange polynomial *)
let compute_polynomial_term : int -> int -> int -> int =
 fun i y_i n ->
  let sign = if (n - i - 1) mod 2 = 0 then 1 else -1 in
  n_choose_k n i * y_i * sign

let interpolate : reading -> int = function
  | r ->
      let n = Array.length r in
      Array.mapi (fun i y_i -> compute_polynomial_term i y_i n) r
      |> (fun arr ->
           Printf.printf "interpolate: %s\n" (Util.string_of_int_array arr) ;
           arr )
      |> Util.sum_array

let compute_poly_term_part_2 : int -> int -> int -> int =
 fun i y_i n ->
  let sign = if i mod 2 = 0 then 1 else -1 in
  n_choose_k n (i + 1) * y_i * sign

let interpolate_part_2 : reading -> int = function
  | r ->
      let n = Array.length r in
      Array.mapi (fun i y_i -> compute_poly_term_part_2 i y_i n) r
      |> Util.sum_array

let solve_part1 : string list -> string = function
  | lst ->
      parse_values lst |> List.map interpolate
      |> (fun l ->
           print_endline (Util.string_of_int_list l) ;
           l )
      |> Util.sum_list |> string_of_int

let solve_part2 : string list -> string = function
  | lst ->
      parse_values lst
      |> List.map interpolate_part_2
      |> (fun l ->
           print_endline (Util.string_of_int_list l) ;
           l )
      |> Util.sum_list |> string_of_int

let solve : string list -> bool -> string =
 fun lst part1 -> if part1 then solve_part1 lst else solve_part2 lst

type input_type = (string * string) list

let parse : string list -> input_type =
 fun lst -> List.map (fun s -> (s, s)) lst

let solve_part_1 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a

let solve_part_2 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a
