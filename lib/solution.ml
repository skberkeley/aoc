module type S = sig
  val solve_part_1 : string list -> string

  val solve_part_2 : string list -> string
end

module type S' = sig
  val solve : string list -> bool -> string
end

module Make (S : S) : S' = struct
  let solve : string list -> bool -> string =
   fun lst part_1 -> if part_1 then S.solve_part_1 lst else S.solve_part_2 lst
end
