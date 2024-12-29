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

(* let rec is_safe_2 ?(verbose = false) ?(is_asc = None) ?(prev = None)
     (lst : int list) =
   let () =
     if verbose then
       Printf.printf "is_safe_2 called with %s, is_asc: %s,\n"
         (Util.string_of_int_list lst)
         (Util.string_of_bool_option is_asc)
   in
   match (lst, is_asc) with
   | [], _ | [_], _ ->
       true
   | a :: b :: lst, is_asc ->
       let diff = b - a in
       let unsafe =
         diff = 0
         || Int.abs diff > 3
         || (Option.is_some is_asc && diff > 0 <> Option.get is_asc)
       in
       if unsafe then
         if Option.is_none prev then
           is_safe ~verbose ~is_asc (b :: lst)
           || is_safe ~verbose ~is_asc (a :: lst)
         else
           let prev = Option.get prev in
           is_safe ~verbose ~is_asc (prev :: b :: lst)
           || is_safe ~verbose ~is_asc (prev :: a :: lst)
       else
         let is_asc =
           if Option.is_some is_asc then is_asc else Some (diff > 0)
         in
         is_safe_2 ~verbose ~is_asc ~prev:(Some a) (b :: lst) *)

let rec is_safe_2 ?(verbose = false) ?(prev = None) (lst : int list) =
  let () =
    if verbose then
      Printf.printf "is_safe_2 called with %s, prev: %s,\n"
        (Util.string_of_int_list lst)
        (Util.string_of_int_list_option prev)
  in
  match (lst, prev) with
  | [], _ ->
      false
  | hd :: tl, None ->
      is_safe tl || is_safe_2 ~verbose ~prev:(Some [hd]) tl
  | hd :: tl, Some l ->
      is_safe (List.rev l @ tl) || is_safe_2 ~verbose ~prev:(Some (hd :: l)) tl

let solve_part_2 : input_type -> string =
 fun lst ->
  (* let b = is_safe_2 ~verbose:true [63; 63; 64; 65; 67; 70; 72] in
     let () = Printf.printf "%B\n" b in *)
  (* let reports =
       List.map (fun r -> (r, is_safe r, is_safe_2 r)) lst
       |> List.filter (fun (_, b, b') -> b <> b')
     in
     let () =
       List.iter
         (fun (r, b, b') ->
           Printf.printf "%s is: %B and %B\n" (Util.string_of_int_list r) b b' )
         reports
     in *)
  List.filter (is_safe_2 ~verbose:false) lst |> List.length |> string_of_int
