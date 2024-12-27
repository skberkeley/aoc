type handful = {red: int; blue: int; green: int}

type game = {id: int; handfuls: handful list}

let bag = {red= 12; green= 13; blue= 14}

let game_re = Str.regexp {|^Game \([0-9]+\): \(.+\)$|}

let color_count_re = Str.regexp {|\([0-9]+\) \([a-z]+\), |}

let color_count_re2 = Str.regexp {|\([0-9]+\) \([a-z]+\);?|}

let handful_valid : handful -> bool =
 fun h -> h.red <= bag.red && h.blue <= bag.blue && h.green <= bag.green

let game_valid : game -> bool = fun g -> List.for_all handful_valid g.handfuls

let parse_color_count : string -> string * int * string =
 fun s ->
  if Str.string_match color_count_re s 0 then
    let n = Str.matched_group 1 s |> int_of_string in
    let color = Str.matched_group 2 s in
    let after = Str.string_after s (String.length (Str.matched_string s)) in
    (color, n, after)
  else
    let _ = Str.string_match color_count_re2 s 0 in
    let n = Str.matched_group 1 s |> int_of_string in
    let color = Str.matched_group 2 s in
    (color, n, "")

let handful_of_string : string -> handful =
 fun s ->
  let rec helper : handful -> string -> handful =
   fun h s ->
    if String.length s = 0 then h
    else
      match parse_color_count s with
      | color, n, s' ->
          if color = "red" then helper {red= n; blue= h.blue; green= h.green} s'
          else if color = "blue" then
            helper {red= h.red; blue= n; green= h.green} s'
          else helper {red= h.red; blue= h.blue; green= n} s'
  in
  helper {red= 0; blue= 0; green= 0} s

let handfuls_of_string : string -> handful list =
 fun s ->
  String.split_on_char ';' s |> List.map String.trim
  |> List.map handful_of_string

let game_of_string : string -> game =
 fun s ->
  if not (Str.string_match game_re s 0) then
    failwith "Game string does not match regex"
  else
    let id = int_of_string (Str.matched_group 1 s) in
    let handfuls = handfuls_of_string (Str.matched_group 2 s) in
    {id; handfuls}

let solve_part1 : string list -> string =
 fun lst ->
  List.map game_of_string lst
  |> List.filter game_valid
  |> List.map (fun g -> g.id)
  |> List.fold_left ( + ) 0 |> string_of_int

let rec min_set_of_handfuls : handful list -> handful -> handful =
 fun lst h ->
  match lst with
  | [] ->
      h
  | {red= r; blue= b; green= g} :: lst ->
      min_set_of_handfuls lst
        {red= max h.red r; blue= max h.blue b; green= max h.green g}

let min_set : game -> handful =
 fun g -> min_set_of_handfuls g.handfuls {red= 0; blue= 0; green= 0}

let power_of_handful : handful -> int = fun h -> h.red * h.blue * h.green

let solve_part2 : string list -> string =
 fun lst ->
  List.map game_of_string lst
  |> List.map min_set |> List.map power_of_handful |> List.fold_left ( + ) 0
  |> string_of_int

let solve : string list -> string = fun lst -> solve_part2 lst

type input_type = (string * string) list

let parse : string list -> input_type =
 fun lst -> List.map (fun s -> (s, s)) lst

let solve_part_1 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a

let solve_part_2 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a
