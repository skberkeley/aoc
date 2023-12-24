open Aoc2023
let day = 7
let test = false
let input_file_name = 
    if test
        then "inputs/day" ^ (string_of_int day) ^ "test"
        else "inputs/day" ^ (string_of_int day)

let read_file : string =
    let ic = open_in input_file_name in
    let file_length = in_channel_length ic in
    try
        let file_string = really_input_string ic file_length in
        close_in ic;
        file_string
    with e ->
        (* some unexpected exception occurs *)
        close_in_noerr ic;
        (* emergency closing *)
        raise e

let split_file_contents : string list =
    String.split_on_char '\n' read_file

let solution =
  match day with
  | 1 -> (Day1.solve split_file_contents)
  | 2 -> (Day2.solve split_file_contents)
  | 3 -> (Day3.solve split_file_contents)
  | 4 -> (Day4.solve split_file_contents)
  | 5 -> (Day5.solve split_file_contents)
  | 6 -> (Day6.solve split_file_contents)
  | 7 -> (Day7.solve split_file_contents)
  (*
  | 8 -> (Day8.solve split_file_contents)
  | 9 -> (Day9.solve split_file_contents)
  | 10 -> (Day10.solve split_file_contents)
  | 11 -> (Day11.solve split_file_contents)
  | 12 -> (Day12.solve split_file_contents)
  | 13 -> (Day13.solve split_file_contents)
  | 14 -> (Day14.solve split_file_contents)
  | 15 -> (Day15.solve split_file_contents)
  | 16 -> (Day16.solve split_file_contents)
  | 17 -> (Day17.solve split_file_contents)
  | 18 -> (Day18.solve split_file_contents)
  | 19 -> (Day19.solve split_file_contents)
  | 20 -> (Day20.solve split_file_contents)
  | 21 -> (Day21.solve split_file_contents)
  | 22 -> (Day22.solve split_file_contents)
  | 23 -> (Day23.solve split_file_contents)
  | 24 -> (Day24.solve split_file_contents)
  | 25 -> (Day25.solve split_file_contents)
*)
  | _ -> "Invalid day"

let () =
  print_endline solution