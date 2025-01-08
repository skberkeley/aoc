open Core
open Poly
module PairSet = Set.Make (Util.IntPair)

type square =
  {height: int; mutable reachable_peaks: PairSet.t; mutable score: int}

type input_type = square array array

let parse (lst : string list) =
  Util.parse_matrix lst (fun c ->
      let height = String.of_char c |> int_of_string in
      {height; reachable_peaks= PairSet.empty; score= 0} )

let verbose = false

type coordinate = {n: int; m: int}

let coordinate_map_of_mat (arr : input_type) =
  let coordinate_map =
    ref
      (Map.of_alist_exn
         (module Int)
         [ (0, [])
         ; (1, [])
         ; (2, [])
         ; (3, [])
         ; (4, [])
         ; (5, [])
         ; (6, [])
         ; (7, [])
         ; (8, [])
         ; (9, []) ] )
  in
  let length = Array.length arr in
  let width = Array.length arr.(0) in
  for i = 0 to length - 1 do
    for j = 0 to width - 1 do
      let h = arr.(i).(j).height in
      let l = Map.find_exn !coordinate_map h in
      coordinate_map := Map.set !coordinate_map ~key:h ~data:({n= i; m= j} :: l)
    done
  done ;
  !coordinate_map

let solve_part_1 (arr : input_type) =
  (* Create a map of height to coordinates *)
  let coordinate_map = coordinate_map_of_mat arr in
  (* For each height, compute the reachable peaks from each square *)
  (* For each square of height 8, their reachable peaks are the adjancent 9's *)
  let length = Array.length arr in
  let width = Array.length arr.(0) in
  List.iter (Map.find_exn coordinate_map 8) ~f:(fun {n; m} ->
      let set = arr.(n).(m).reachable_peaks in
      let set =
        if n > 0 && arr.(n - 1).(m).height = 9 then Set.add set (n - 1, m)
        else set
      in
      let set =
        if n < length - 1 && arr.(n + 1).(m).height = 9 then
          Set.add set (n + 1, m)
        else set
      in
      let set =
        if m > 0 && arr.(n).(m - 1).height = 9 then Set.add set (n, m - 1)
        else set
      in
      let set =
        if m < width - 1 && arr.(n).(m + 1).height = 9 then
          Set.add set (n, m + 1)
        else set
      in
      arr.(n).(m).reachable_peaks <- set ;
      if verbose then
        Printf.printf "(%d, %d) has height %d and reachable peaks %s\n" n m
          arr.(n).(m).height
          (PairSet.sexp_of_t set |> Sexp.to_string_hum) ) ;
  (* For each square of height n from 7 to 0, *)
  (* union the reachable peaks of adjacent squares with height n - 1 *)
  for height = 7 downto 0 do
    List.iter (Map.find_exn coordinate_map height) ~f:(fun {n; m} ->
        let set = arr.(n).(m).reachable_peaks in
        let set =
          if n > 0 && arr.(n - 1).(m).height = height + 1 then
            Set.union set arr.(n - 1).(m).reachable_peaks
          else set
        in
        let set =
          if n < length - 1 && arr.(n + 1).(m).height = height + 1 then
            Set.union set arr.(n + 1).(m).reachable_peaks
          else set
        in
        let set =
          if m > 0 && arr.(n).(m - 1).height = height + 1 then
            Set.union set arr.(n).(m - 1).reachable_peaks
          else set
        in
        let set =
          if m < width - 1 && arr.(n).(m + 1).height = height + 1 then
            Set.union set arr.(n).(m + 1).reachable_peaks
          else set
        in
        arr.(n).(m).reachable_peaks <- set ;
        if verbose then
          Printf.printf "(%d, %d) has height %d and reachable peaks %s\n" n m
            arr.(n).(m).height
            (PairSet.sexp_of_t set |> Sexp.to_string_hum) )
  done ;
  List.fold_left (Map.find_exn coordinate_map 0) ~init:0 ~f:(fun acc {n; m} ->
      if verbose then
        Printf.printf "Trailhead score: %d\n"
          (Set.length arr.(n).(m).reachable_peaks) ;
      acc + Set.length arr.(n).(m).reachable_peaks )
  |> string_of_int

let solve_part_2 (arr : input_type) =
  (* Create a map of height to coordinates *)
  let coordinate_map = coordinate_map_of_mat arr in
  (* For each height, compute scores *)
  (* For each square of height 8, their score is the number of adjancent 9's *)
  let length = Array.length arr in
  let width = Array.length arr.(0) in
  List.iter (Map.find_exn coordinate_map 8) ~f:(fun {n; m} ->
      let score =
        (if n > 0 && arr.(n - 1).(m).height = 9 then 1 else 0)
        + (if n < length - 1 && arr.(n + 1).(m).height = 9 then 1 else 0)
        + (if m > 0 && arr.(n).(m - 1).height = 9 then 1 else 0)
        + if m < width - 1 && arr.(n).(m + 1).height = 9 then 1 else 0
      in
      arr.(n).(m).score <- score ;
      if verbose then
        Printf.printf "(%d, %d) has height %d and score %d\n" n m
          arr.(n).(m).height score ) ;
  (* For each square of height n from 7 to 0, *)
  (* its score is the sum of scores of adjacent squares with height n + 1 *)
  for height = 7 downto 0 do
    List.iter (Map.find_exn coordinate_map height) ~f:(fun {n; m} ->
        let score =
          ( if n > 0 && arr.(n - 1).(m).height = height + 1 then
              arr.(n - 1).(m).score
            else 0 )
          + ( if n < length - 1 && arr.(n + 1).(m).height = height + 1 then
                arr.(n + 1).(m).score
              else 0 )
          + ( if m > 0 && arr.(n).(m - 1).height = height + 1 then
                arr.(n).(m - 1).score
              else 0 )
          +
          if m < width - 1 && arr.(n).(m + 1).height = height + 1 then
            arr.(n).(m + 1).score
          else 0
        in
        arr.(n).(m).score <- score ;
        if verbose then
          Printf.printf "(%d, %d) has height %d and score %d\n" n m
            arr.(n).(m).height score )
  done ;
  List.fold_left (Map.find_exn coordinate_map 0) ~init:0 ~f:(fun acc {n; m} ->
      if verbose then Printf.printf "Trailhead score: %d\n" arr.(n).(m).score ;
      acc + arr.(n).(m).score )
  |> string_of_int
