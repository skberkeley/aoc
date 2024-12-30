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

let get_traversed_positions : input_type -> PairsSet.t = function
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
      with Break -> !visited )

let solve_part_1 (i : input_type) =
  PairsSet.cardinal (get_traversed_positions i) |> string_of_int

module PairsMap = Map.Make (IntPairs)

exception Answer of bool

let char_of_dir = function N -> '^' | E -> '>' | S -> 'V' | W -> '<'

let print_guard_and_map (i : input_type) (block : position) =
  let map, (pos, dir) = i in
  let map = Array.copy map in
  let i, j = pos in
  map.(i).(j) <- char_of_dir dir ;
  let i, j = block in
  map.(i).(j) <- 'O' ;
  let width = Array.length map.(0) in
  print_endline (String.make width '=') ;
  Array.iter (fun arr -> print_endline (Array.to_seq arr |> String.of_seq)) map ;
  print_endline (String.make width '=')

let check_loop ?(verbose = false) (i : input_type) (block : position) =
  let seen_obstacles = ref PairsMap.empty in
  let map, (pos, dir) = i in
  let curr_pos = ref pos in
  let curr_dir = ref dir in
  let length = Array.length map in
  let width = Array.length map.(0) in
  try
    while true do
      if verbose then print_guard_and_map (map, (!curr_pos, !curr_dir)) block ;
      (* compute next position *)
      let next_i, next_j = compute_next !curr_pos !curr_dir in
      (* if next is oob, return false *)
      match (next_i, next_j) with
      | -1, _ | _, -1 ->
          raise (Answer false)
      | i, _ when i = length ->
          raise (Answer false)
      | _, j when j = width ->
          raise (Answer false)
      | _, _ ->
          (* if next is obstacle, add to seen and turn (don't step) *)
          if map.(next_i).(next_j) = '#' || (next_i, next_j) = block then (
            let next_pos = (next_i, next_j) in
            if PairsMap.mem next_pos !seen_obstacles then
              if List.mem !curr_dir (PairsMap.find next_pos !seen_obstacles)
              then raise (Answer true)
              else
                seen_obstacles :=
                  PairsMap.add next_pos
                    (!curr_dir :: PairsMap.find next_pos !seen_obstacles)
                    !seen_obstacles
            else
              seen_obstacles :=
                PairsMap.add next_pos [!curr_dir] !seen_obstacles ;
            curr_dir := next_dir !curr_dir )
          else curr_pos := (next_i, next_j)
    done ;
    failwith "Execution shouldn't reach here"
  with Answer b -> b

let solve_part_2 (i : input_type) =
  (* for each traversed position, put a block, and check if it would result in a loop *)
  let original_traversed = get_traversed_positions i in
  (* let _ = check_loop ~verbose:true i (127, 37) in *)
  let loop_points = PairsSet.filter (check_loop i) original_traversed in
  (* PairsSet.iter (fun (n1, n2) -> Printf.printf "(%d, %d)\n" n1 n2) loop_points ; *)
  let _, (pos, _) = i in
  Printf.printf "%B\n" (PairsSet.mem pos loop_points) ;
  loop_points |> PairsSet.cardinal |> string_of_int
