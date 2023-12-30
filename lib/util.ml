let string_of_int_list : int list -> string =
  function lst ->
    List.map string_of_int lst
    |> String.concat ", "
    |> (fun s -> "[" ^ s ^ "]")