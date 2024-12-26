type input_type = int * int

let parse : string list -> (int * int) list =
 fun lst ->
  let nums =
    List.map
      (fun s -> List.map int_of_string (Str.split (Str.regexp {| +|}) s))
      lst
  in
  List.map
    (fun n ->
      match n with
      | [n1; n2] ->
          (n1, n2)
      | _ ->
          raise (Invalid_argument "Unexpected number format") )
    nums

let solve_part_1 : input_type list -> string =
 fun lst ->
  let nums1, nums2 = List.split lst in
  let nums1 = List.sort ( - ) nums1 in
  let nums2 = List.sort ( - ) nums2 in
  string_of_int
    (List.fold_left2 (fun acc a b -> acc + Int.abs (a - b)) 0 nums1 nums2)

module IntMap = Map.Make (Int)

let solve_part_2 : input_type list -> string =
 fun lst ->
  let nums1, nums2 = List.split lst in
  let counts = IntMap.empty in
  let counts =
    List.fold_left
      (fun counts n ->
        match IntMap.find_opt n counts with
        | Some n' ->
            IntMap.add n (n' + 1) counts
        | None ->
            IntMap.add n 1 counts )
      counts nums2
  in
  let muls =
    List.map
      (fun n ->
        match IntMap.find_opt n counts with Some n' -> n * n' | None -> 0 )
      nums1
  in
  string_of_int (List.fold_left ( + ) 0 muls)
