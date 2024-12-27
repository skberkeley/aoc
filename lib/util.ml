let string_of_int_list : int list -> string = function
  | lst ->
      List.map string_of_int lst |> String.concat ", " |> fun s -> "[" ^ s ^ "]"

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
