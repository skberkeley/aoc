(* type maze = char array array
   type interval =
     | Complete of int * int
     | Incomplete of int
   type coords = int * int
   let compare_coords : coords -> coords -> int =
     fun c1 c2 ->
       match c1, c2 with
       | (x1, y1), (x2, y2) ->
         let c = compare x1 x2 in
         if c = 0 then compare y1 y2 else c

   let parse_maze : string list -> maze =
     function lst ->
       List.map (String.to_bytes) lst
       |> List.map Util.char_array_of_bytes
       |> Array.of_list

   (* find the starting point i, j of the maze where i is the row number and j the position in that row *)
   let find_start : maze -> (int * int) =
     function m ->
       Array.mapi
         (fun i' row ->
           match Util.array_index_of_opt 'S' row with
           | Some j' -> Some (i', j')
           | None -> None
         )
         m
       |> Array.fold_left
         (fun so_far curr ->
           match so_far with
           | Some _ -> so_far
           | None -> curr)
         None
       |> Option.get

   let get_symbol_from_maze : maze -> int * int -> char =
     fun m coords ->
       let i, j = coords in
       m.(i).(j)

   let compute_next_coords : maze -> int * int -> int * int -> int * int =
     fun m curr prev ->
       let s = get_symbol_from_maze m curr in
       match s, curr with
       | '|', (i, j) when prev = (i - 1, j) -> i + 1, j
       | '|', (i, j) when prev = (i + 1, j) -> i - 1, j
       | '-', (i, j) when prev = (i, j - 1) -> i, j + 1
       | '-', (i, j) when prev = (i, j + 1) -> i, j - 1
       | 'L', (i, j) when prev = (i - 1, j) -> i, j + 1
       | 'L', (i, j) when prev = (i, j + 1) -> i - 1, j
       | 'J', (i, j) when prev = (i - 1, j) -> i, j - 1
       | 'J', (i, j) when prev = (i, j - 1) -> i - 1, j
       | '7', (i, j) when prev = (i, j - 1) -> i + 1, j
       | '7', (i, j) when prev = (i + 1, j) -> i, j - 1
       | 'F', (i, j) when prev = (i, j + 1) -> i + 1, j
       | 'F', (i, j) when prev = (i + 1, j) -> i, j + 1
       | _ -> failwith "unexpected symbol in maze"

   let compute_next_coords_from_start : maze -> int * int -> int * int =
     fun m start ->
       let i, j = start in
       let s = get_symbol_from_maze m (i - 1, j) in
       if s = '|' || s = '7' || s = 'F' then
         i - 1, j
       else
       let s = get_symbol_from_maze m (i, j + 1) in
       if s = '-' || s = 'J' || s = '7' then
         i, j + 1
       else
       let s = get_symbol_from_maze m (i + 1, j) in
       if s = '|' || s = 'L' || s = 'J' then
         i + 1, j
       else
       i, j - 1

   (* iterates over the loop contained in the maze. at each cell part of the loop, calls
      f on the coordinates of that cell *)
   let iterate_over_loop : maze -> (coords -> unit) -> unit =
     fun m f ->
       let start = find_start m in
       let prev : (int * int) ref = ref start in
       let curr : (int * int) ref = ref (compute_next_coords_from_start m start) in
       while !curr <> start do
         f !prev;
         let curr' = compute_next_coords m !curr !prev in
         prev := !curr;
         curr := curr';
       done

   let find_loop_length : maze -> int =
     function m ->
       let loop_length : int ref = ref 1 in
       let f = function _ -> loop_length := !loop_length + 1 in
       iterate_over_loop m f;
       !loop_length

   let get_loop_coords : maze -> coords list =
     function m ->
       let loop_coords = ref [] in
       let f = function c -> loop_coords := c :: !loop_coords in
       iterate_over_loop m f;
       !loop_coords


   (* replaces all cells in the maze not part of the loop with a "." *)
   let filter_maze_for_loop : maze -> maze =
     function m ->
       let loop_coords = List.sort compare_coords (get_loop_coords m) in
       let height = Array.length m in
       let width = Array.length m.(0) in
       let rec helper =
         fun m i j height width loop_coords ->
           let loop_coords =
             match loop_coords with
               | (x, y) :: loop_coords when x = i && y = j ->
                 loop_coords
               | _ ->
                 m.(i).(j) <- '.';
                 loop_coords
           in
           match i, j with
           | i, j when i = height - 1 && j = width - 1 ->
             m
           | i, j when j = width - 1 ->
             helper m (i + 1) 0 height width loop_coords
           | _ ->
             helper m i (j + 1) height width loop_coords
       in
       helper m 0 0 height width loop_coords

   let intervals_of_maze_row : char array -> interval list =
     function arr ->
       let intervals = ref [] in
       let f =
         fun i c ->
           match c, !intervals with
           | '|', Incomplete x :: rst -> intervals := Complete (x, i) :: rst
           | '|', _ -> intervals := Incomplete i :: !intervals
           | '-', _ -> ()
           | 'L', Incomplete x :: rst -> intervals := Complete (x, i) :: rst
           | 'L', _ -> ()

       failwith "todo"

   let intervals_of_maze : maze -> interval list list =
     function m ->
       let m = filter_maze_for_loop m in
       Array.map intervals_of_maze_row m
       |> Array.to_list

   let num_inside_intervals : interval list -> int =
     function lst ->
       failwith "todo"

   let solve_part1 : string list -> string =
     function lst ->
       parse_maze lst
       |> find_loop_length
       |> (fun i -> i / 2)
       |> string_of_int

   let solve_part2 : string list -> string =
     function lst ->
       parse_maze lst
       |> intervals_of_maze
       |> List.map num_inside_intervals
       |> Util.sum_list
       |> string_of_int

   let solve : string list -> bool -> string =
     fun lst part1 ->
       if part1 then solve_part1 lst else solve_part2 lst *)

type input_type = (string * string) list

let parse : string list -> input_type =
 fun lst -> List.map (fun s -> (s, s)) lst

let solve_part_1 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a

let solve_part_2 : input_type -> string =
 fun lines -> match List.hd lines with a, _ -> a
