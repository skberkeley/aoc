type input_type = int list list

let parse : string list -> input_type =
 fun lst -> List.map Util.parse_int_list lst

let rec is_safe ?(verbose = false) ?(is_asc = None) lst =
  let () =
    if verbose then
      Printf.printf "is_safe called with %s and %s\n"
        (Util.string_of_int_list lst)
        (Util.string_of_bool_option is_asc)
  in
  match (lst, is_asc) with
  | [], _ | [_], _ ->
      true
  | a :: b :: lst', None ->
      let diff = Int.abs (a - b) in
      if diff = 0 || diff > 3 then false
      else
        let is_asc' = b - a > 0 in
        is_safe ~verbose ~is_asc:(Some is_asc') (b :: lst')
  | a :: b :: lst', Some is_asc ->
      let diff = b - a in
      if diff = 0 || diff > 0 <> is_asc then false
      else if Int.abs diff > 3 then false
      else is_safe ~verbose ~is_asc:(Some is_asc) (b :: lst')

let solve_part_1 : input_type -> string =
 fun lst -> List.filter is_safe lst |> List.length |> string_of_int

let solve_part_2 : input_type -> string = fun _ -> failwith "todo"
