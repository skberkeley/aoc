type node_id = string
type node = node_id * node_id
module NodeMap = Map.Make(String)
type network = node NodeMap.t
type direction = L | R
type path = direction Seq.t

let parse_path : string list -> path * string list =
  function lst ->
    let path_string = List.hd lst in
    let lst = List.tl lst |> List.tl in
    let p = String.to_seq path_string 
    |> Seq.map (function |'L' -> L |'R' -> R |_ -> failwith "unrecognized character")
    |> Seq.cycle in
    p, lst

let node_regex = Str.regexp {|\([A-Z]+\) = (\([A-Z]+\), \([A-Z]+\))|}
let parse_network_entry : string -> node_id * node =
  function s ->
    if not (Str.string_match node_regex s 0) then failwith "node_regex didn't match" else
    Str.matched_group 1 s, (Str.matched_group 2 s, Str.matched_group 3 s)

let parse_network : string list -> network =
  function lst ->
    List.map parse_network_entry lst
    |> List.to_seq
    |> NodeMap.of_seq

let parse_puzzle : string list -> path * network =
  function lst ->
    let path, lst = parse_path lst in
    path, parse_network lst

let take_step : node_id -> direction -> network -> node_id =
  fun id d n ->
    match d, NodeMap.find id n with
    | L, (id', _) | R, (_, id') -> id'

let num_steps : path * network -> int =
  function p, n ->
    let rec helper : node_id -> path -> int -> int =
      fun id p i ->
        if id = "ZZZ" then i else
        match p () with
        | Cons (d, p) -> 
          let next = take_step id d n in
          helper next p (i + 1)
        | _ -> failwith "num_steps: path ran out"
    in
    helper "AAA" p 0

let solve_part1 : string list -> string =
  function lst ->
    parse_puzzle lst 
    |> num_steps
    |> string_of_int

let solve_part2 : string list -> string =
  function _ ->
    failwith "todo"

let solve : string list -> bool -> string =
  fun lst part1 ->
    if part1 then solve_part1 lst else solve_part2 lst