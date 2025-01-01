open Core
open Poly
module CharMap = Hashtbl.Make (Char)

type pair = int * int

type input_type = {antennas: pair list CharMap.t; length: int; width: int}

let parse (lst : string list) =
  let arr = List.map lst ~f:String.to_array |> Array.of_list in
  let antennas = ref (CharMap.create ()) in
  let length = Array.length arr in
  let width = Array.length arr.(0) in
  for i = 0 to length - 1 do
    for j = 0 to length - 1 do
      let c = arr.(i).(j) in
      if c <> '.' then
        if Hashtbl.mem !antennas c then
          let l = Hashtbl.find_exn !antennas c in
          Hashtbl.set !antennas ~key:c ~data:((i, j) :: l)
        else
          let _ = Hashtbl.add !antennas ~key:c ~data:[(i, j)] in
          ()
    done
  done ;
  {antennas= !antennas; length; width}

module PairSet = Hash_set.Make (Util.IntPair)

let compute_antinodes (length : int) (width : int) (antennas : pair list) =
  let antinodes = PairSet.create () in
  let arr = Array.of_list antennas in
  let num_antennas = Array.length arr in
  for i = 0 to num_antennas - 2 do
    for j = i + 1 to num_antennas - 1 do
      let (x1, y1), (x2, y2) = (arr.(i), arr.(j)) in
      let dx, dy = (x2 - x1, y2 - y1) in
      (* check for antinodes in between *)
      if dx mod 3 = 0 && dy mod 3 = 0 then (
        Hash_set.add antinodes (x1 + (dx / 3), y1 + (dy / 3)) ;
        Hash_set.add antinodes (x2 - (dx / 3), y2 - (dy / 3)) ) ;
      Hash_set.add antinodes (x1 - dx, y1 - dy) ;
      Hash_set.add antinodes (x2 + dx, y2 + dy)
    done
  done ;
  Hash_set.filter antinodes ~f:(fun (x, y) ->
      x >= 0 && x < length && y >= 0 && y < width )

let solve_part_1 (i : input_type) =
  let antinodes =
    Hashtbl.fold i.antennas ~init:(PairSet.create ())
      ~f:(fun ~key:_ ~data:antennas antinodes ->
        Hash_set.union antinodes (compute_antinodes i.length i.width antennas) )
  in
  (* Hash_set.iter antinodes ~f:(fun (x, y) -> Printf.printf "(%d, %d)\n" x y) ; *)
  (* Hash_set.iter
     (compute_antinodes i.length i.width (Hashtbl.find_exn i.antennas '0'))
     ~f:(fun (x, y) -> Printf.printf "(%d, %d)\n" x y) ; *)
  Hash_set.length antinodes |> string_of_int

let compute_antinodes (length : int) (width : int) (antennas : pair list) =
  let is_in_bounds (x : int) (y : int) =
    x >= 0 && x < length && y >= 0 && y < width
  in
  let antinodes = PairSet.create () in
  let arr = Array.of_list antennas in
  let num_antennas = Array.length arr in
  for i = 0 to num_antennas - 2 do
    for j = i + 1 to num_antennas - 1 do
      let (x1, y1), (x2, y2) = (arr.(i), arr.(j)) in
      let dx, dy = (x2 - x1, y2 - y1) in
      (* compute step *)
      let gcd = Util.gcd dx dy in
      let step_x, step_y = (dx / gcd, dy / gcd) in
      let curr_x, curr_y = (ref x1, ref y1) in
      while is_in_bounds !curr_x !curr_y do
        Hash_set.add antinodes (!curr_x, !curr_y) ;
        curr_x := !curr_x + step_x ;
        curr_y := !curr_y + step_y
      done ;
      let curr_x, curr_y = (ref x1, ref y1) in
      while is_in_bounds !curr_x !curr_y do
        Hash_set.add antinodes (!curr_x, !curr_y) ;
        curr_x := !curr_x - step_x ;
        curr_y := !curr_y - step_y
      done
    done
  done ;
  antinodes

let solve_part_2 (i : input_type) =
  let antinodes =
    Hashtbl.fold i.antennas ~init:(PairSet.create ())
      ~f:(fun ~key:_ ~data:antennas antinodes ->
        Hash_set.union antinodes (compute_antinodes i.length i.width antennas) )
  in
  Hash_set.length antinodes |> string_of_int
