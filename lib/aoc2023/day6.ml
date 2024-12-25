type race = int * int

let times_regex = Str.regexp {|^Time: +\([0-9 ]+\)$|}

let distance_regex = Str.regexp {|^Distance: +\([0-9 ]+\)$|}

(* parses a white-space separated list of ints into a list *)
let int_list_of_string : string -> int list =
 fun s ->
  String.split_on_char ' ' s
  |> List.filter (fun s -> String.length s > 0)
  |> List.map int_of_string

let parse_races : string list -> race list =
 fun lst ->
  if List.length lst <> 2 then failwith "input was of unexpected length"
  else
    let time_string = List.hd lst in
    let distance_string = List.hd (List.tl lst) in
    if not (Str.string_match times_regex time_string 0) then
      Printf.sprintf "time_regex didn't match %s\n" time_string |> failwith
    else if not (Str.string_match distance_regex distance_string 0) then
      Printf.sprintf "distance_regex didn't match %s\n" distance_string
      |> failwith
    else
      let times = Str.matched_group 1 time_string |> int_list_of_string in
      let distances =
        Str.matched_group 1 distance_string |> int_list_of_string
      in
      List.combine times distances

let num_winning_ways : race -> int =
 fun r ->
  (*
     t = total time
     h = time held
     d = total distance
     r = record
     d = (t - h) * h
     d > r
     t -+ sqrt(t^2 - 4r) / 2 = 0
     compute when total distance is equal to needed
     round as necessary
     compute number of winning ways *)
  let t, r = r in
  Printf.printf "race: %d, %d\n" t r ;
  let determinant = (t * t) - (4 * r) in
  if determinant < 0 then 0
  else
    let root = sqrt (float determinant) in
    let zero1 = (float t -. root) /. 2.0 in
    let zero2 = (float t +. root) /. 2.0 in
    let zero1 = if zero1 = ceil zero1 then zero1 +. 1.0 else zero1 in
    let zero2 = if zero1 = floor zero2 then zero2 -. 1.0 else zero2 in
    let zero1 = int_of_float zero1 |> max 0 in
    let zero2 = int_of_float zero2 |> min t in
    Printf.printf "num_winning_ways: zero1: %d zero2: %d\n" zero1 zero2 ;
    Printf.printf "num_winning_ways: %d\n" (zero2 - zero1 + 1) ;
    zero2 - zero1

let solve_part1 : string list -> string =
 fun lst ->
  parse_races lst |> List.map num_winning_ways |> List.fold_left ( * ) 1
  |> string_of_int

let int_of_spaced_string : string -> int = function
  | s ->
      String.split_on_char ' ' s
      |> List.filter (fun s -> String.length s > 0)
      |> String.concat "" |> int_of_string

let parse_race : string list -> race = function
  | [time_string; distance_string] ->
      if not (Str.string_match times_regex time_string 0) then
        Printf.sprintf "time_regex didn't match %s\n" time_string |> failwith
      else if not (Str.string_match distance_regex distance_string 0) then
        Printf.sprintf "distance_regex didn't match %s\n" distance_string
        |> failwith
      else
        let time = Str.matched_group 1 time_string |> int_of_spaced_string in
        let distance =
          Str.matched_group 1 distance_string |> int_of_spaced_string
        in
        (time, distance)
  | _ ->
      failwith "input was of unexpected length"

let solve_part2 : string list -> string =
 fun lst -> parse_race lst |> num_winning_ways |> string_of_int

let solve : string list -> string = fun lst -> solve_part2 lst
