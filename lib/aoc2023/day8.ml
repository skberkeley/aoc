open Util

type node_id = string

type node = node_id * node_id

module NodeMap = Map.Make (String)

type network = node NodeMap.t

type direction = L | R

type path = direction Seq.t

let parse_path : string list -> path * string list = function
  | lst ->
      let path_string = List.hd lst in
      let lst = List.tl lst |> List.tl in
      let p =
        String.to_seq path_string
        |> Seq.map (function
             | 'L' ->
                 L
             | 'R' ->
                 R
             | _ ->
                 failwith "unrecognized character" )
        |> Seq.cycle
      in
      (p, lst)

let node_regex = Str.regexp {|\([0-9A-Z]+\) = (\([0-9A-Z]+\), \([0-9A-Z]+\))|}

let parse_network_entry : string -> node_id * node = function
  | s ->
      if not (Str.string_match node_regex s 0) then
        failwith "node_regex didn't match"
      else
        (Str.matched_group 1 s, (Str.matched_group 2 s, Str.matched_group 3 s))

let parse_network : string list -> network = function
  | lst ->
      List.map parse_network_entry lst |> List.to_seq |> NodeMap.of_seq

let parse_puzzle : string list -> path * network = function
  | lst ->
      let path, lst = parse_path lst in
      (path, parse_network lst)

let take_step : direction -> network -> node_id -> node_id =
 fun d n id ->
  match (d, NodeMap.find id n) with L, (id', _) | R, (_, id') -> id'

let num_steps_from_id : path -> network -> node_id -> int =
 fun p n id ->
  let rec helper : node_id -> path -> int -> int =
   fun id p i ->
    if String.ends_with ~suffix:"Z" id then i
    else
      match p () with
      | Cons (d, p) ->
          let next = take_step d n id in
          helper next p (i + 1)
      | _ ->
          failwith "num_steps: path ran out"
  in
  helper id p 0

let num_steps : path * network -> int = function
  | p, n ->
      num_steps_from_id p n "AAA"

let solve_part1 : string list -> string = function
  | lst ->
      parse_puzzle lst |> num_steps |> string_of_int

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match (m, n) with 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / gcd m n

let lcm_of_list : int list -> int = function
  | n :: lst ->
      List.fold_left lcm n lst
  | _ ->
      1

let num_ghost_steps : path * network -> int = function
  | p, n ->
      let starting_ids =
        NodeMap.filter (fun id _ -> String.ends_with ~suffix:"A" id) n
        |> NodeMap.bindings
        |> List.map (function id, _ -> id)
      in
      let nums = starting_ids |> List.map (num_steps_from_id p n) in
      print_endline (string_of_int_list nums) ;
      lcm_of_list nums

let solve_part2 : string list -> string = function
  | lst ->
      parse_puzzle lst |> num_ghost_steps |> string_of_int

let solve : string list -> bool -> string =
 fun lst part1 -> if part1 then solve_part1 lst else solve_part2 lst

type input_type = (string * string) list

let parse : string list -> input_type =
 fun lst -> List.map (fun s -> (s, s)) lst

let solve_part_1 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a

let solve_part_2 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a
