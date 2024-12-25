let parse : string list -> int list list =
 fun lst ->
  List.map (fun s -> List.map int_of_string (String.split_on_char ' ' s)) lst

let solve_part_1 : string list -> string = fun lst -> List.hd lst

let solve_part_2 : string list -> string = fun s -> List.hd s
