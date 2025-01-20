open Core
open Poly

type input_type = char array array

let parse (lst : string list) = Util.parse_matrix lst Fun.id

let verbose = false

let solve_part_1 (l : input_type) =
  let length = Array.length l in
  let width = Array.length l.(0) in
  (* Do a DFS to fully explore each region *)
  (* Maintain a set of seen coordinates across the regions *)
  let seen = Util.PairSet.create () in
  let total_price = ref 0 in
  for i = 0 to length - 1 do
    for j = 0 to width - 1 do
      if not (Hash_set.mem seen (i, j)) then (
        let area = ref 0 in
        let perimeter = ref 0 in
        let c = l.(i).(j) in
        let fringe = Util.PairSet.create () in
        Hash_set.add fringe (i, j) ;
        while not (Hash_set.is_empty fringe) do
          let i', j' =
            Hash_set.find fringe ~f:(fun _ -> true) |> Option.value_exn
          in
          if verbose then
            Printf.printf "Fringe: %s\nProcessing (%d, %d)\n"
              ( Hash_set.sexp_of_t Util.IntPair.sexp_of_t fringe
              |> Sexp.to_string_hum )
              i' j' ;
          Hash_set.remove fringe (i', j') ;
          if not (Hash_set.mem seen (i', j')) then (
            Hash_set.add seen (i', j') ;
            (* When processing a square, increment the region's area *)
            area := !area + 1 ;
            (* For each adjacent square, either increment perimeter or add it to the fringe *)
            if i' > 0 && l.(i' - 1).(j') = c then
              Hash_set.add fringe (i' - 1, j')
            else perimeter := !perimeter + 1 ;
            if i' < length - 1 && l.(i' + 1).(j') = c then
              Hash_set.add fringe (i' + 1, j')
            else perimeter := !perimeter + 1 ;
            if j' > 0 && l.(i').(j' - 1) = c then
              Hash_set.add fringe (i', j' - 1)
            else perimeter := !perimeter + 1 ;
            if j' < width - 1 && l.(i').(j' + 1) = c then
              Hash_set.add fringe (i', j' + 1)
            else perimeter := !perimeter + 1 )
        done ;
        if verbose then
          Printf.printf "Region %c has area %d and perimeter %d\n" c !area
            !perimeter ;
        total_price := !total_price + (!area * !perimeter) )
    done
  done ;
  !total_price |> string_of_int

let solve_part_2 (l : input_type) =
  let length = Array.length l in
  let width = Array.length l.(0) in
  (* Do a DFS to fully explore each region *)
  (* Maintain a set of seen coordinates across the regions *)
  let seen = Util.PairSet.create () in
  let total_price = ref 0 in
  for i = 0 to length - 1 do
    for j = 0 to width - 1 do
      if not (Hash_set.mem seen (i, j)) then (
        let area = ref 0 in
        let sides = ref 0 in
        let c = l.(i).(j) in
        let fringe = Util.PairSet.create () in
        Hash_set.add fringe (i, j) ;
        while not (Hash_set.is_empty fringe) do
          let i', j' =
            Hash_set.find fringe ~f:(fun _ -> true) |> Option.value_exn
          in
          if verbose then
            Printf.printf "Fringe: %s\nProcessing (%d, %d)\n"
              ( Hash_set.sexp_of_t Util.IntPair.sexp_of_t fringe
              |> Sexp.to_string_hum )
              i' j' ;
          Hash_set.remove fringe (i', j') ;
          if not (Hash_set.mem seen (i', j')) then (
            Hash_set.add seen (i', j') ;
            (* When processing a square, increment the region's area *)
            area := !area + 1 ;
            (* For each adjacent square, check whether to add it to the fringe *)
            if i' > 0 && l.(i' - 1).(j') = c then
              Hash_set.add fringe (i' - 1, j') ;
            if i' < length - 1 && l.(i' + 1).(j') = c then
              Hash_set.add fringe (i' + 1, j') ;
            if j' > 0 && l.(i').(j' - 1) = c then
              Hash_set.add fringe (i', j' - 1) ;
            if j' < width - 1 && l.(i').(j' + 1) = c then
              Hash_set.add fringe (i', j' + 1) ;
            (* Check whether each side is a new side *)
            let here = l.(i').(j') in
            (* above: check if here != up && here = left && here != up left *)
            (* or: on upper border and here = left *)
            if
              (i' = 0 || here <> l.(i' - 1).(j'))
              && not
                   ( i' > 0 && j' > 0
                     && here <> l.(i' - 1).(j')
                     && here = l.(i').(j' - 1)
                     && here <> l.(i' - 1).(j' - 1)
                   || (i' = 0 && j' > 0 && here = l.(i').(j' - 1)) )
            then (
              if verbose then Printf.printf "up side: (%d, %d)\n" i' j' ;
              sides := !sides + 1 ) ;
            (* below: check if here != down && here = right && here != down right *)
            (* or: on lower border and here = right *)
            if
              (i' = length - 1 || here <> l.(i' + 1).(j'))
              && not
                   ( i' < length - 1
                     && j' < width - 1
                     && here <> l.(i' + 1).(j')
                     && here = l.(i').(j' + 1)
                     && here <> l.(i' + 1).(j' + 1)
                   || i' = length - 1
                      && j' < width - 1
                      && here = l.(i').(j' + 1) )
            then (
              if verbose then Printf.printf "down side: (%d, %d)\n" i' j' ;
              sides := !sides + 1 ) ;
            (* left: check if here != left && here = down && here != down left *)
            (* or: on left border and here = down *)
            if
              (j' = 0 || here <> l.(i').(j' - 1))
              && not
                   ( i' < length - 1
                     && j' > 0
                     && here <> l.(i').(j' - 1)
                     && here = l.(i' + 1).(j')
                     && here <> l.(i' + 1).(j' - 1)
                   || (j' = 0 && i' < length - 1 && here = l.(i' + 1).(j')) )
            then (
              if verbose then Printf.printf "left side: (%d, %d)\n" i' j' ;
              sides := !sides + 1 ) ;
            (* right: check if here != right && here = up && here != up right *)
            (* or: on right border and here = up *)
            if
              (j' = width - 1 || here <> l.(i').(j' + 1))
              && not
                   ( i' > 0
                     && j' < width - 1
                     && here <> l.(i').(j' + 1)
                     && here = l.(i' - 1).(j')
                     && here <> l.(i' - 1).(j' + 1)
                   || (j' = width - 1 && i' > 0 && here = l.(i' - 1).(j')) )
            then (
              if verbose then Printf.printf "right side: (%d, %d)\n" i' j' ;
              sides := !sides + 1 ) )
        done ;
        if verbose then
          Printf.printf "Region %c has area %d and %d sides\n" c !area !sides ;
        total_price := !total_price + (!area * !sides) )
    done
  done ;
  !total_price |> string_of_int
