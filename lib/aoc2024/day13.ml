open Core
open Poly

type button = {dx: int; dy: int}

let string_of_button (b : button) =
  Printf.sprintf "{dx = %d; dy = %d}" b.dx b.dy

type prize = {x: int; y: int}

let string_of_prize (p : prize) = Printf.sprintf "{x = %d; y = %d}" p.x p.y

type machine = {a: button; b: button; prize: prize}

let string_of_machine (m : machine) =
  Printf.sprintf "{a = %s; b = %s; prize = %s}" (string_of_button m.a)
    (string_of_button m.b) (string_of_prize m.prize)

type input_type = machine list

let a_re = Str.regexp {|^Button A: X\+\([0-9]+\), Y\+\([0-9]+\)$|}

let b_re = Str.regexp {|^Button B: X\+\([0-9]+\), Y\+\([0-9]+\)$|}

let prize_re = Str.regexp {|^Prize: X=\([0-9]+\), Y=\([0-9]+\)$|}

let parse_button (lst : string list) =
  match lst with
  | a :: b :: prize :: _ :: lst ->
      if not (Str.string_match a_re a 0) then (
        Printf.printf "%s\n" a ;
        failwith "Button A regex failed" ) ;
      let a =
        { dx= int_of_string (Str.matched_group 1 a)
        ; dy= int_of_string (Str.matched_group 2 a) }
      in
      if not (Str.string_match b_re b 0) then failwith "Button B regex failed" ;
      let b =
        { dx= int_of_string (Str.matched_group 1 b)
        ; dy= int_of_string (Str.matched_group 2 b) }
      in
      if not (Str.string_match prize_re prize 0) then
        failwith "Prize regex failed" ;
      let prize =
        { x= int_of_string (Str.matched_group 1 prize)
        ; y= int_of_string (Str.matched_group 2 prize) }
      in
      ({a; b; prize}, lst)
  | _ ->
      failwith "Unexpected lst structure"

let rec parse (lst : string list) =
  let machine, lst = parse_button lst in
  if List.is_empty lst then [machine] else machine :: parse lst

let verbose = false

let solve_parallel (_ : machine) = failwith "todo"

let solve_machine ?(verbose = false) ?(part1 = true) (m : machine) =
  (* Check if determinant is 0 *)
  let {dx= ax; dy= ay} = m.a in
  let {dx= bx; dy= by} = m.b in
  let {x= px; y= py} = m.prize in
  let det = (ay * bx) - (ax * by) in
  if det = 0 then
    (* Check if the lines intersect the sol'n *)
    if float_of_int px /. float_of_int py = float_of_int ax /. float_of_int ay
    then
      (* If they do, compute the cheapest number of button clicks *)
      solve_parallel m
    else (
      if verbose then
        Printf.printf "%s\nDeterminant 0, but lines don't intersect prize\n"
          (string_of_machine m) ;
      0 )
  else if
    (* Check if b has an integer solution *)
    ((ay * px) - (ax * py)) mod det <> 0
  then (
    if verbose then
      Printf.printf "%s\nb doesn't have an integer solution\n"
        (string_of_machine m) ;
    0 )
  else
    let b' = ((ay * px) - (ax * py)) / det in
    if part1 && b' > 100 then 0
    else if (* Check if a has an integer solution *)
            (px - (bx * b')) mod ax <> 0
    then (
      if verbose then
        Printf.printf "%s\na doesn't have an integer solution\n"
          (string_of_machine m) ;
      0 )
    else
      let a' = (px - (bx * b')) / ax in
      if part1 && a' > 100 then 0 else (* Return 3a + b *)
                                    (3 * a') + b'

let solve_part_1 (l : input_type) =
  List.map l ~f:solve_machine |> Util.sum_list |> string_of_int

let solve_part_2 (l : input_type) =
  List.map l ~f:(fun {a; b; prize= {x; y}} ->
      {a; b; prize= {x= 10000000000000 + x; y= 10000000000000 + y}} )
  |> List.map ~f:(solve_machine ~part1:false)
  |> Util.sum_list |> string_of_int
