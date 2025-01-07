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

type free_space = {index: int; size: int}

let string_of_free_space = function
  | {index; size} ->
      Printf.sprintf "{index= %d; size = %d}" index size

let rec free_spaces_of_diskmap ?(curr_index = 0) (disk_map : int list) =
  match disk_map with
  | [] | [_] ->
      []
  | a :: b :: disk_map ->
      {index= curr_index + a; size= b}
      :: free_spaces_of_diskmap ~curr_index:(curr_index + a + b) disk_map

type file_2 = {id: int; size: int; index: int}

let file_2s_of_diskmap (disk_map : int list) =
  let l = ref [] in
  let rec helper (curr_index : int) (num_files : int) (disk_map : int list) =
    match disk_map with
    | [] ->
        ()
    | [a] ->
        l := {id= num_files; size= a; index= curr_index} :: !l
    | a :: b :: disk_map ->
        l := {id= num_files; size= a; index= curr_index} :: !l ;
        helper (curr_index + a + b) (num_files + 1) disk_map
  in
  helper 0 0 disk_map ; !l

let rec find_free_space ?(verbose = false) (free_spaces : free_space list)
    (size : int) (file_index : int) =
  (* returns index of free space to use, and resulting free space list *)
  (* the returned index is -1 if no free spaces are large enough *)
  if verbose then
    Printf.printf "free_spaces: %s, size: %d\n"
      (Util.string_of_list free_spaces string_of_free_space)
      size ;
  match free_spaces with
  | [] ->
      (-1, [])
  | {index; _} :: free_spaces when index > file_index ->
      (-1, free_spaces)
  | {index; size= fs_size} :: free_spaces when size = fs_size ->
      (index, free_spaces)
  | {index; size= fs_size} :: free_spaces when size < fs_size ->
      (index, {index= index + size; size= fs_size - size} :: free_spaces)
  | fs :: free_spaces ->
      (* size > fs_size *)
      let i, free_spaces = find_free_space free_spaces size file_index in
      (i, fs :: free_spaces)

let verbose = false

let solve_part_2 (i : input_type) =
  (* maintain a list of each free space's size and index, in their original order *)
  let free_spaces = ref (free_spaces_of_diskmap i.disk_map) in
  let files = file_2s_of_diskmap i.disk_map in
  (* for each file, from right to left, figure out which free space it would go in, *)
  (* compute the resulting checksum contribution, and update the free space list *)
  let checksum = ref 0 in
  List.iter files ~f:(fun f ->
      let i, fs = find_free_space ~verbose !free_spaces f.size f.index in
      free_spaces := fs ;
      let i = if i >= 0 then i else f.index in
      if verbose then
        Printf.printf "{id = %d; size = %d; index = %d}\n" f.id f.size i ;
      checksum := !checksum + (f.id * partial_sum i f.size) ) ;
  !checksum |> string_of_int
