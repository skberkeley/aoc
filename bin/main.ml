open Aoc

let year = 2024

let day = 6

(* let test = true *)
let test = false

let part1 = true

(* let part1 = false *)

let input_file_name =
  "inputs/" ^ string_of_int year
  ^ (if test then "/test" else "/full")
  ^ "/day" ^ string_of_int day

let read_file : string =
  let ic = open_in input_file_name in
  let file_length = in_channel_length ic in
  try
    let file_string = really_input_string ic file_length in
    close_in ic ; file_string
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic ;
    (* emergency closing *)
    raise e

let split_file_contents : string list = String.split_on_char '\n' read_file

module type SOLN = sig
  type input_type

  val parse : string list -> input_type

  val solve_part_1 : input_type -> string

  val solve_part_2 : input_type -> string
end

module type YEAR = sig
  module Day1 : SOLN

  module Day2 : SOLN

  module Day3 : SOLN

  module Day4 : SOLN

  module Day5 : SOLN

  module Day6 : SOLN
end

let get_year_module (year : int) =
  match year with
  | 2023 ->
      (module Aoc2023 : YEAR)
  | 2024 ->
      (module Aoc2024 : YEAR)
  | _ ->
      raise (Invalid_argument "Invalid year")

module YearModule = (val get_year_module year)

let get_day_module (day : int) =
  match day with
  | 1 ->
      (module YearModule.Day1 : SOLN)
  | 2 ->
      (module YearModule.Day2 : SOLN)
  | 3 ->
      (module YearModule.Day3 : SOLN)
  | 4 ->
      (module YearModule.Day4 : SOLN)
  | 5 ->
      (module YearModule.Day5 : SOLN)
  | 6 ->
      (module YearModule.Day6 : SOLN)
  | _ ->
      raise (Invalid_argument "Invalid day")

module DayModule = (val get_day_module day)

let solution = if part1 then DayModule.solve_part_1 else DayModule.solve_part_2

let () = print_endline (solution (DayModule.parse split_file_contents))
