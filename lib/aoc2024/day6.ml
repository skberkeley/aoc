type direction = N | S | E | W

type position = int * int

type guard = position * direction

type map = char array array

type input_type = map * guard

let parse : string list -> input_type =
 fun lst ->
  let arrs =
    List.map (fun s -> String.to_seq s |> Array.of_seq) lst |> Array.of_list
  in
  let position = ref (0, 0) in
  Array.iteri
    (fun i arr ->
      match Array.find_index (fun c -> c = '^') arr with
      | None ->
          ()
      | Some j ->
          position := (i, j) )
    arrs ;
  (arrs, (!position, N))

module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module PairsSet = Set.Make (IntPairs)

exception Break

let compute_next (curr : position) (dir : direction) =
  match curr with
  | i, j -> (
    match dir with
    | N ->
        (i - 1, j)
    | E ->
        (i, j + 1)
    | S ->
        (i + 1, j)
    | W ->
        (i, j - 1) )

let next_dir (d : direction) = match d with N -> E | E -> S | S -> W | W -> N

let solve_part_1 : input_type -> string = function
  | map, (pos, dir) -> (
      let visited = ref PairsSet.empty in
      try
        let curr_pos = ref pos in
        let curr_dir = ref dir in
        let length = Array.length map in
        let width = Array.length map.(0) in
        while true do
          (* add current position to visited *)
          visited := PairsSet.add !curr_pos !visited ;
          (* compute next position *)
          let next_i, next_j = compute_next !curr_pos !curr_dir in
          (* if next is oob, break *)
          match (next_i, next_j) with
          | -1, _ | _, -1 ->
              raise Break
          | i, _ when i = length ->
              raise Break
          | _, j when j = width ->
              raise Break
          | _, _ ->
              (* if next is obstacle, turn (don't step) *)
              if map.(next_i).(next_j) = '#' then curr_dir := next_dir !curr_dir
              else curr_pos := (next_i, next_j)
        done ;
        failwith "Execution shouldn't reach here"
      with Break -> PairsSet.cardinal !visited |> string_of_int )

let solve_part_2 : input_type -> string = function _ -> failwith "todo"
