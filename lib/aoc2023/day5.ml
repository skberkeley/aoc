type range = {start: int; length: int}

module Ranges = struct
  type t = range

  let compare : t -> t -> int =
   fun r1 r2 ->
    match Stdlib.compare r1.start r2.start with
    | 0 ->
        Stdlib.compare r1.length r2.length
    | c ->
        c
end

module Range_map = Map.Make (Ranges)

type range_map = range Range_map.t

let string_of_range : range -> string = function
  | {start; length} ->
      Printf.sprintf "(%d, %d)" start length

let seeds_re = Str.regexp {|seeds: \([0-9 ]+\)|}

let parse_seeds : string -> int list =
 fun s ->
  if Str.string_match seeds_re s 0 then
    Str.matched_group 1 s |> String.trim |> String.split_on_char ' '
    |> List.map int_of_string
  else failwith (Printf.sprintf "Seeds regex didn't match %s" s)

let map_name_re = Str.regexp {|\([a-z]+-to-[a-z]+\) map:|}

let parse_map_name : string list -> string * string list =
 fun lst ->
  match lst with
  | s :: lst ->
      if Str.string_match map_name_re s 0 then (Str.matched_group 1 s, lst)
      else failwith (Printf.sprintf "Map name regex didn't match %s" s)
  | _ ->
      failwith "Tried to parse map name from empty input"

let range_re = Str.regexp {|\([0-9]+\) \([0-9]+\) \([0-9]+\)|}

let parse_range : string list -> (range * range) option * string list =
 fun lst ->
  match lst with
  | s :: lst ->
      if Str.string_match range_re s 0 then
        let dst_start = Str.matched_group 1 s |> int_of_string in
        let src_start = Str.matched_group 2 s |> int_of_string in
        let length = Str.matched_group 3 s |> int_of_string in
        (Some ({start= src_start; length}, {start= dst_start; length}), lst)
      else (None, lst)
  | _ ->
      (None, [])

let rec parse_ranges : string list -> (range * range) list * string list =
 fun lst ->
  match parse_range lst with
  | Some r, lst ->
      let ranges, lst = parse_ranges lst in
      (r :: ranges, lst)
  | None, lst ->
      ([], lst)

let parse_range_map : string list -> range_map * string list =
 fun lst ->
  let _, lst = parse_map_name lst in
  let ranges, lst = parse_ranges lst in
  let map = List.to_seq ranges |> Range_map.of_seq in
  (map, lst)

let rec parse_range_maps : string list -> range_map list =
 fun lst ->
  match parse_range_map lst with
  | m, [] ->
      [m]
  | m, lst ->
      m :: parse_range_maps lst

let parse_seeds_and_maps : string list -> int list * range_map list =
 fun lst ->
  let seeds = parse_seeds (List.hd lst) in
  let maps = parse_range_maps (List.tl (List.tl lst)) in
  (seeds, maps)

let rec map_int_with_bindings : (range * range) list -> int -> int =
 fun lst i ->
  match lst with
  | [] ->
      i
  | ({start= src_start; length= src_len}, {start= dst_start; length= _}) :: rst
    ->
      if i < src_start then i
      else if i < src_start + src_len then dst_start + (i - src_start)
      else map_int_with_bindings rst i

let map_int : range_map -> int -> int =
 fun m i -> map_int_with_bindings (Range_map.bindings m) i

let rec composed_map : range_map list -> int -> int =
 fun maps i ->
  match maps with [] -> i | m :: rst -> composed_map rst (map_int m i)

let solve_part1 : string list -> string =
 fun lst ->
  let seeds, maps = parse_seeds_and_maps lst in
  List.map (composed_map maps) seeds
  |> List.fold_left min Int.max_int
  |> string_of_int

let rec range_list_of_int_list : int list -> range list = function
  | start :: length :: rst ->
      {start; length} :: range_list_of_int_list rst
  | [] ->
      []
  | _ ->
      failwith "odd number of elements"

let parse_seed_ranges : string -> range list =
 fun s ->
  if Str.string_match seeds_re s 0 then
    Str.matched_group 1 s |> String.trim |> String.split_on_char ' '
    |> List.map int_of_string |> range_list_of_int_list
  else failwith (Printf.sprintf "Seeds regex didn't match %s" s)

let parse_src_ranges_and_maps : string list -> range list * range_map list =
 fun lst ->
  let seed_ranges = parse_seed_ranges (List.hd lst) in
  let maps = parse_range_maps (List.tl (List.tl lst)) in
  (seed_ranges, maps)

let rec map_range_with_bindings :
    (range * range) list -> range list -> range list =
 fun bindings ranges ->
  List.map
    (function
      | r1, r2 ->
          Printf.sprintf "%s -> %s" (string_of_range r1) (string_of_range r2) )
    bindings
  |> String.concat " "
  |> Printf.printf "bindings: %s\n" ;
  List.map string_of_range ranges
  |> String.concat " "
  |> Printf.printf "ranges: %s\n" ;
  match (bindings, ranges) with
  | [], _ | _, [] ->
      ranges
  | ( ({start= src_start; length= map_range_length}, {start= dst_start; _})
      :: b_rst
    , {start= to_map_start; length= to_map_length} :: r_rst ) ->
      let src_end = src_start + map_range_length - 1 in
      let to_map_end = to_map_start + to_map_length - 1 in
      if src_start < to_map_start then
        if src_end < to_map_start then
          (* ranges are disjoint *)
          map_range_with_bindings b_rst ranges
        else if src_end < to_map_end then
          (* some overlap *)
          { start= dst_start + to_map_start - src_start
          ; length= src_end - to_map_start + 1 }
          :: map_range_with_bindings b_rst
               ( { start= src_end + 1
                 ; length= to_map_length - (src_end - to_map_start + 1) }
               :: r_rst )
        else if src_end = to_map_end then
          (* endings align *)
          {start= dst_start + to_map_start - src_start; length= to_map_length}
          :: map_range_with_bindings b_rst r_rst
        else
          (* to_map is contained in src *)
          {start= dst_start + to_map_start - src_start; length= to_map_length}
          :: map_range_with_bindings bindings r_rst
      else if src_start = to_map_start then
        if src_end < to_map_end then
          (* src is contained in to_map *)
          {start= dst_start; length= map_range_length}
          :: map_range_with_bindings b_rst
               ( { start= to_map_start + map_range_length
                 ; length= to_map_length - map_range_length }
               :: r_rst )
        else if src_end = to_map_end then
          (* src and to_map are identical *)
          {start= dst_start; length= map_range_length}
          :: map_range_with_bindings b_rst r_rst
        else
          (* to_map is contained in src *)
          {start= dst_start; length= to_map_length}
          :: map_range_with_bindings bindings r_rst
      else if src_start < to_map_end then
        if src_end < to_map_end then
          (* src is contained in to_map *)
          {start= to_map_start; length= src_start - to_map_start}
          :: {start= dst_start; length= map_range_length}
          :: map_range_with_bindings b_rst
               ({start= src_end + 1; length= to_map_end - src_end} :: r_rst)
        else if src_end = to_map_end then
          (* endings align *)
          {start= to_map_start; length= src_start - to_map_start}
          :: {start= dst_start; length= map_range_length}
          :: map_range_with_bindings b_rst r_rst
        else
          (* src extends past end of to_map *)
          {start= to_map_start; length= src_start - to_map_start}
          :: {start= dst_start; length= to_map_end - src_start + 1}
          :: map_range_with_bindings bindings r_rst
      else if src_start = to_map_end then
        {start= to_map_start; length= to_map_length - 1}
        :: {start= dst_start; length= 1}
        :: map_range_with_bindings bindings r_rst
      else
        (* src_start > to_map_end *)
        {start= to_map_start; length= to_map_length}
        :: map_range_with_bindings bindings r_rst

(* ranges is a sorted, disjoint list of ranges
   returns the sorted, disjoint list of ranges resulting from mapping each input range*)
let map_range : range list -> range_map -> range list =
 fun ranges m ->
  let ranges_total_length =
    List.map (function {length; _} -> length) ranges |> List.fold_left ( + ) 0
  in
  let mapped =
    map_range_with_bindings (Range_map.bindings m) ranges
    |> List.sort Ranges.compare
  in
  let mapped_total_length =
    List.map (function {length; _} -> length) mapped |> List.fold_left ( + ) 0
  in
  if ranges_total_length = mapped_total_length then mapped
  else failwith "mapped range lengths didn't total to original range lengths"

(* ranges is a sorted, disjoint list of ranges
   returns the sorted, disjoint list of ranges resulting from mapping each input range*)
let composed_map_range : range_map list -> range -> range list =
 fun maps range ->
  let r = List.fold_left map_range [range] maps in
  List.length r |> string_of_int |> print_endline ;
  List.map string_of_range r |> String.concat " " |> print_endline ;
  r

let solve_part2 : string list -> string =
 fun lst ->
  let seed_ranges, maps = parse_src_ranges_and_maps lst in
  let seed_ranges = List.sort Ranges.compare seed_ranges in
  List.map (composed_map_range maps)
    seed_ranges (* map each input range to a list of ranges *)
  |> List.map List.hd (* get the first (smallest) range of each list *)
  |> List.map (fun {start; _} ->
         Printf.printf "smallest location: %d\n" start ;
         start )
     (* get each range's start *)
  |> List.fold_left min Int.max_int (* find the smallest *)
  |> string_of_int

let solve : string list -> string = fun lst -> solve_part2 lst

type input_type = (string * string) list

let parse : string list -> input_type =
 fun lst -> List.map (fun s -> (s, s)) lst

let solve_part_1 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a

let solve_part_2 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a
