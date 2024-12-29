type input_type = char array array

let parse : string list -> input_type =
 fun lst -> List.map String.to_seq lst |> List.map Array.of_seq |> Array.of_list

type direction = N | NE | E | SE | S | SW | W | NW

let get_adj (i : int) (j : int) (max_i : int) (max_j : int) =
  let ret = ref [] in
  if i > 0 && j > 0 then ret := (i - 1, j - 1, NW) :: !ret ;
  if j > 0 then ret := (i, j - 1, W) :: !ret ;
  if i < max_i && j > 0 then ret := (i + 1, j - 1, SW) :: !ret ;
  if i < max_i then ret := (i + 1, j, S) :: !ret ;
  if i < max_i && j < max_j then ret := (i + 1, j + 1, SE) :: !ret ;
  if j < max_j then ret := (i, j + 1, E) :: !ret ;
  if i > 0 && j < max_j then ret := (i - 1, j + 1, NE) :: !ret ;
  if i > 0 then ret := (i - 1, j, N) :: !ret ;
  !ret

let get_next_char (c : char) =
  match c with
  | 'M' ->
      'A'
  | 'A' ->
      'S'
  | _ ->
      raise (Invalid_argument "Unsupported character")

let step (i : int) (j : int) (d : direction) (max_i : int) (max_j : int) =
  let next_i =
    match d with
    | N | NE | NW ->
        if i > 0 then Some (i - 1) else None
    | W | E ->
        Some i
    | SW | S | SE ->
        if i < max_i then Some (i + 1) else None
  in
  let next_j =
    match d with
    | NW | W | SW ->
        if j > 0 then Some (j - 1) else None
    | N | S ->
        Some j
    | NE | E | SE ->
        if j < max_j then Some (j + 1) else None
  in
  match (next_i, next_j) with
  | None, _ | _, None ->
      None
  | Some i, Some j ->
      Some (i, j)

let solve_part_1 : input_type -> string =
 fun arr ->
  let count = ref 0 in
  let length = Array.length arr in
  let width = Array.length arr.(0) in
  for i = 0 to length - 1 do
    for j = 0 to width - 1 do
      if arr.(i).(j) = 'X' then
        let to_check =
          ref
            ( get_adj i j (length - 1) (width - 1)
            |> List.map (fun (n1, n2, d) -> (n1, n2, d, 'M')) )
        in
        while !to_check <> [] do
          let curr_i, curr_j, curr_d, curr_c = List.hd !to_check in
          to_check := List.tl !to_check ;
          if arr.(curr_i).(curr_j) = curr_c then
            if curr_c = 'S' then count := !count + 1
            else
              match step curr_i curr_j curr_d (length - 1) (width - 1) with
              | Some (next_i, next_j) ->
                  to_check :=
                    (next_i, next_j, curr_d, get_next_char curr_c) :: !to_check
              | None ->
                  ()
        done
    done
  done ;
  !count |> string_of_int

let solve_part_2 : input_type -> string =
 fun arr ->
  let count = ref 0 in
  let length = Array.length arr in
  let width = Array.length arr.(0) in
  for i = 1 to length - 2 do
    for j = 1 to width - 2 do
      if arr.(i).(j) = 'A' then
        let diag_1 = [arr.(i - 1).(j - 1); arr.(i + 1).(j + 1)] in
        let diag_2 = [arr.(i + 1).(j - 1); arr.(i - 1).(j + 1)] in
        if
          (diag_1 = ['M'; 'S'] || diag_1 = ['S'; 'M'])
          && (diag_2 = ['M'; 'S'] || diag_2 = ['S'; 'M'])
        then count := !count + 1
    done
  done ;
  !count |> string_of_int
