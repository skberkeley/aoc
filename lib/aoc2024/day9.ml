open Core
open Poly

type file = {id: int; size: int}

let string_of_file (f : file) =
  Printf.sprintf "{id = %d; size = %d}" f.id f.size

let string_of_file_deque (d : file Deque.t) =
  Printf.sprintf "["
  ^ Deque.fold d ~init:"" ~f:(fun s f -> s ^ string_of_file f)
  ^ "]"

type input_type = {disk_map: int list; files: file Deque.t}

let parse (lst : string list) =
  let i = List.hd_exn lst in
  let disk_map =
    String.to_list i |> List.map ~f:(fun c -> String.of_char c |> int_of_string)
  in
  let files = Deque.create ~initial_length:(List.length disk_map / 2) () in
  List.iteri disk_map ~f:(fun i size ->
      if i mod 2 = 0 then Deque.enqueue_back files {id= Deque.length files; size} ) ;
  {disk_map; files}

let partial_sum (index : int) (num_blocks : int) =
  num_blocks * (index + index + num_blocks - 1) / 2

let solve_part_1 (i : input_type) =
  let checksum = ref 0 in
  let rec helper ?(verbose = false) (disk_map : int list) (is_file : bool)
      (index : int) =
    if verbose then
      Printf.printf "disk_map: %s\n files: %s\n is_file: %B\n index: %d\n"
        (Util.string_of_int_list disk_map)
        (string_of_file_deque i.files)
        is_file index ;
    if Deque.is_empty i.files then ()
    else
      match (disk_map, is_file) with
      | [], _ ->
          failwith "disk map should never be empty"
      | n :: disk_map, true ->
          let f = Deque.dequeue_front_exn i.files in
          let amt = f.id * partial_sum index f.size in
          checksum := !checksum + amt ;
          if verbose then
            Printf.printf "%d blocks of %d\nadding %d to checksum\n" f.size f.id
              amt ;
          helper ~verbose disk_map false (index + n)
      | n :: disk_map, false when (Deque.peek_back_exn i.files).size < n ->
          let f = Deque.dequeue_back_exn i.files in
          let amt = f.id * partial_sum index f.size in
          checksum := !checksum + amt ;
          if verbose then
            Printf.printf "%d blocks of %d\nadding %d to checksum\n" f.size f.id
              amt ;
          helper ~verbose ((n - f.size) :: disk_map) false (index + f.size)
      | n :: disk_map, false when (Deque.peek_back_exn i.files).size > n ->
          let f = Deque.dequeue_back_exn i.files in
          let amt = f.id * partial_sum index n in
          checksum := !checksum + amt ;
          if verbose then
            Printf.printf "%d blocks of %d\nadding %d to checksum\n" n f.id amt ;
          Deque.enqueue_back i.files {id= f.id; size= f.size - n} ;
          helper ~verbose disk_map true (index + n)
      | n :: disk_map, false ->
          (* f.size = n *)
          let f = Deque.dequeue_back_exn i.files in
          let amt = f.id * partial_sum index n in
          checksum := !checksum + amt ;
          if verbose then
            Printf.printf "%d blocks of %d\nadding %d to checksum\n" f.size f.id
              amt ;
          helper ~verbose disk_map true (index + n)
  in
  helper ~verbose:false i.disk_map true 0 ;
  !checksum |> string_of_int

let solve_part_2 (_ : input_type) = failwith "todo"
