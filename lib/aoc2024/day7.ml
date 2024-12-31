type equation = int * int list

type input_type = equation list

let re = Str.regexp {|^\([0-9]+\):\([ 0-9]+\)$|}

let spaces = Str.regexp {|[ ]+|}

let parse_equation (s : string) =
  if not (Str.string_match re s 0) then failwith "Could not parse equation"
  else
    let n = Str.matched_group 1 s |> int_of_string in
    let ops =
      Str.matched_group 2 s |> Str.split spaces |> List.map int_of_string
      |> List.rev
    in
    (n, ops)

let parse : string list -> input_type = fun lst -> List.map parse_equation lst

let rec check = function
  | _, [] | _, [_] ->
      failwith "These cases should never be reached"
  | n, [a; b] ->
      n = a + b || n = a * b
  | n, _ when n < 0 ->
      false
  | n, a :: lst ->
      if n mod a <> 0 then check (n - a, lst)
      else check (n - a, lst) || check (n / a, lst)

let solve_part_1 (i : input_type) =
  List.filter check i
  |> List.map (fun (i, _) -> i)
  |> Util.sum_list |> string_of_int

let concat (n1 : int) (n2 : int) =
  int_of_string (string_of_int n1 ^ string_of_int n2)

let check_2 ?(verbose = false) (e : equation) =
  let n, lst = e in
  let rec helper (interm : int) (ops : int list) =
    if verbose then
      Printf.printf "helper called with %d and %s\n" interm
        (Util.string_of_int_list ops) ;
    match ops with
    | [] ->
        n = interm
    | a :: ops ->
        helper (interm + a) ops
        || helper (interm * a) ops
        || helper (concat interm a) ops
  in
  let lst = List.rev lst in
  helper (List.hd lst) (List.tl lst)

let solve_part_2 (i : input_type) =
  (* List.filter (check_2 ~verbose:true) i
     |> List.iter (fun (n, l) ->
            Printf.printf "%d: %s\n" n (Util.string_of_int_list l) ) ; *)
  List.filter check_2 i
  |> List.map (fun (i, _) -> i)
  |> Util.sum_list |> string_of_int
