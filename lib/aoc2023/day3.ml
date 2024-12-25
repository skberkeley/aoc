(* to_bytes_array transforms the input string list into an array of bytes *)
let to_bytes_array : string list -> bytes array =
 fun lst -> List.map String.to_bytes lst |> Array.of_list

let is_digit = function '0' .. '9' -> true | _ -> false

let is_symbol = function '0' .. '9' | '.' -> false | _ -> true

let rec nth_power_of_10 = function 0 -> 1 | n -> 10 * nth_power_of_10 (n - 1)

(* get_num_at_coords returns the number beginning at the specified index and its length
   in the passed bytes *)
let get_num_at_coords : bytes -> int -> int * int =
 fun arr j ->
  let rec helper : bytes -> int * int =
   fun b ->
    if not (is_digit (Bytes.get b 0)) then (0, 0)
    else if Bytes.length b = 1 then
      ((Bytes.get b 0 |> Char.code) - Char.code '0', 1)
    else
      let n = (Bytes.get b 0 |> Char.code) - Char.code '0' in
      match helper (Bytes.sub b 1 (Bytes.length b - 1)) with
      | _, 0 ->
          (n, 1)
      | n', l ->
          ((n * nth_power_of_10 l) + n', l + 1)
  in
  helper (Bytes.sub arr j (Bytes.length arr - j))

(* returns whether there is a symbol adjacent to the number of length len
   beginning at the specified coordinates (i, j) *)
let adjacent_symbol_present : bytes array -> int -> int -> int -> bool =
 fun arr i j len ->
  let symbol_found : bool ref = ref false in
  ( if i > 0 then
      let row = Array.get arr (i - 1) in
      for j' = max 0 (j - 1) to min (j + len) (Bytes.length row - 1) do
        let c = Bytes.get row j' in
        symbol_found := !symbol_found || is_symbol c
      done ) ;
  let row = Array.get arr i in
  ( if j > 0 then
      let c = Bytes.get row (j - 1) in
      symbol_found := !symbol_found || is_symbol c ) ;
  ( if j + len < Bytes.length row then
      let c = Bytes.get row (j + len) in
      symbol_found := !symbol_found || is_symbol c ) ;
  ( if i + 1 < Array.length arr then
      let row = Array.get arr (i + 1) in
      for j' = max 0 (j - 1) to min (j + len) (Bytes.length row - 1) do
        let c = Bytes.get row j' in
        symbol_found := !symbol_found || is_symbol c
      done ) ;
  !symbol_found

(* get_parts_numbers returns a list of part numbers, defined as those numbers
   which are adjacent to a symbol *)
let get_parts_numbers : bytes array -> int list =
 fun arr ->
  let nums : int list ref = ref [] in
  for i = 0 to Array.length arr - 1 do
    let row = Array.get arr i in
    let digits_remaining : int ref = ref 0 in
    for j = 0 to Bytes.length row - 1 do
      Printf.printf "(%d, %d\n)" i j ;
      if !digits_remaining <> 0 then digits_remaining := !digits_remaining - 1
      else if is_digit (Bytes.get row j) then (
        let num, length = get_num_at_coords row j in
        Printf.printf "num: %d, length: %d\n" num length ;
        if adjacent_symbol_present arr i j length then nums := num :: !nums ;
        digits_remaining := length - 1 )
    done
  done ;
  !nums

let solve_part1 : string list -> string =
 fun lst ->
  let part_numbers = to_bytes_array lst |> get_parts_numbers in
  List.iter (fun n -> print_int n ; print_newline ()) part_numbers ;
  part_numbers |> List.fold_left ( + ) 0 |> string_of_int

type gear = int * int

let get_num : bytes array -> int -> int -> int =
 fun arr i j ->
  Printf.printf "get_num called with %d, %d\n" i j ;
  let row = arr.(i) in
  let curr_index : int ref = ref j in
  let num_start_found : bool ref = ref false in
  while not !num_start_found do
    if !curr_index > 0 && is_digit (Bytes.get row (!curr_index - 1)) then
      curr_index := !curr_index - 1
    else num_start_found := true
  done ;
  match get_num_at_coords row !curr_index with
  | n, _ ->
      Printf.printf "get_num: %d\n" n ;
      n

let find_adjacent_nums : bytes array -> int -> int -> int list =
 fun arr i j ->
  let nums : int list ref = ref [] in
  (* row before: *)
  if i > 0 then (
    let row = Array.get arr (i - 1) in
    let has_digit : bool array ref = ref (Array.make 2 false) in
    if j - 1 > 0 then
      if is_digit (Bytes.get row (j - 1)) then (
        nums := get_num arr (i - 1) (j - 1) :: !nums ;
        !has_digit.(0) <- true ) ;
    if is_digit (Bytes.get row j) then (
      !has_digit.(1) <- true ;
      if not !has_digit.(0) then nums := get_num arr (i - 1) j :: !nums ) ;
    if is_digit (Bytes.get row (j + 1)) then
      if not !has_digit.(1) then nums := get_num arr (i - 1) (j + 1) :: !nums ) ;
  (* current row *)
  let row = Array.get arr i in
  let has_digit : bool array ref = ref (Array.make 2 false) in
  if j - 1 > 0 then
    if is_digit (Bytes.get row (j - 1)) then (
      nums := get_num arr i (j - 1) :: !nums ;
      !has_digit.(0) <- true ) ;
  if is_digit (Bytes.get row j) then (
    !has_digit.(1) <- true ;
    if not !has_digit.(0) then nums := get_num arr i j :: !nums ) ;
  if is_digit (Bytes.get row (j + 1)) then
    if not !has_digit.(1) then nums := get_num arr i (j + 1) :: !nums ;
  (* row ahead *)
  if i + 1 < Array.length arr then (
    let row = Array.get arr (i + 1) in
    let has_digit : bool array ref = ref (Array.make 2 false) in
    if j - 1 > 0 then
      if is_digit (Bytes.get row (j - 1)) then (
        nums := get_num arr (i + 1) (j - 1) :: !nums ;
        !has_digit.(0) <- true ) ;
    if is_digit (Bytes.get row j) then (
      !has_digit.(1) <- true ;
      if not !has_digit.(0) then nums := get_num arr (i + 1) j :: !nums ) ;
    if is_digit (Bytes.get row (j + 1)) then
      if not !has_digit.(1) then nums := get_num arr (i + 1) (j + 1) :: !nums ) ;
  !nums

let find_gears : bytes array -> gear list =
 fun arr ->
  let gears : gear list ref = ref [] in
  for i = 0 to Array.length arr - 1 do
    let row = Array.get arr i in
    for j = 0 to Bytes.length row - 1 do
      if Bytes.get row j = '*' then
        let adj_nums = find_adjacent_nums arr i j in
        match adj_nums with
        | [a; b] ->
            Printf.printf "numbers found: %d, %d\n" a b ;
            gears := (a, b) :: !gears
        | _ ->
            ()
    done
  done ;
  !gears

let solve_part2 : string list -> string =
 fun lst ->
  let gears = to_bytes_array lst |> find_gears in
  List.iter (function a, b -> Printf.printf "gear: %d, %d\n" a b) gears ;
  List.map (function a, b -> a * b) gears
  |> List.fold_left ( + ) 0 |> string_of_int

let solve : string list -> string = fun lst -> solve_part2 lst
