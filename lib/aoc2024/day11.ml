open Core
open Poly

type input_type = int list

let parse (lst : string list) = List.hd_exn lst |> Util.parse_int_list

let verbose = false

let solve (l : input_type) (num_iter : int) =
  (* Memoize intermediate results on number and number of iterations left *)
  let memo = Hashtbl.create (module Util.IntPair) in
  let rec helper (n : int) (num_iter : int) =
    if num_iter = 0 then 1
    else if Hashtbl.mem memo (n, num_iter) then
      Hashtbl.find_exn memo (n, num_iter)
    else
      let result =
        if n = 0 then helper 1 (num_iter - 1)
        else
          let s = string_of_int n in
          let len = String.length s in
          if len mod 2 = 0 then
            let len = len / 2 in
            helper (int_of_string (String.sub s ~pos:0 ~len)) (num_iter - 1)
            + helper (int_of_string (String.sub s ~pos:len ~len)) (num_iter - 1)
          else helper (n * 2024) (num_iter - 1)
      in
      Hashtbl.add_exn memo ~key:(n, num_iter) ~data:result ;
      result
  in
  List.fold l ~init:0 ~f:(fun acc n -> acc + helper n num_iter) |> string_of_int

let solve_part_1 (l : input_type) = solve l 25

let solve_part_2 (l : input_type) = solve l 75
