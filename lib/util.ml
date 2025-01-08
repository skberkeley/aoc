let string_of_list : 'a list -> ('a -> string) -> string =
 fun l f -> List.map f l |> String.concat "; " |> fun s -> "[" ^ s ^ "]"

let string_of_int_list : int list -> string = function
  | lst ->
      string_of_list lst string_of_int

let string_of_int_array : int array -> string = function
  | arr ->
      Array.map string_of_int arr
      |> Array.to_list |> String.concat ", "
      |> fun s -> "[" ^ s ^ "]"

let string_of_bool_option : bool option -> string = function
  | None ->
      "None"
  | Some b ->
      string_of_bool b

let string_of_int_list_option : int list option -> string = function
  | None ->
      "None"
  | Some l ->
      string_of_int_list l

let sum_list : int list -> int = function lst -> List.fold_left ( + ) 0 lst

let sum_array : int array -> int = function arr -> Array.fold_left ( + ) 0 arr

let array_index_of_opt : 'a -> 'a array -> int option =
 fun x arr ->
  Array.mapi (fun i x' -> if x = x' then Some i else None) arr
  |> Array.fold_left
       (fun so_far curr -> match so_far with Some _ -> so_far | None -> curr)
       None

let char_array_of_bytes : bytes -> char array =
 fun b ->
  let n = Bytes.length b in
  let arr = ref (Array.make n '0') in
  for i = 0 to n - 1 do
    !arr.(i) <- Bytes.get b i
  done ;
  !arr

(* from rosetta code *)
let factorial ?(verbose = false) n =
  let rec helper i accum =
    if i > n then accum else helper (i + 1) (accum * i)
  in
  let f = helper 1 1 in
  if verbose then Printf.printf "factorial: %d! = %d\n" n f ;
  f

let parse_int_list : string -> int list =
 fun s ->
  let re = Str.regexp {| +|} in
  let nums = Str.split re s in
  List.map int_of_string nums

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

open Core

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Core.Comparable.Make (T)
end

let parse_matrix (l : string list) (char_parser : char -> 'a) =
  List.map l ~f:(fun s -> String.to_array s |> Array.map ~f:char_parser)
  |> Array.of_list
