type hand_type = | Five_oak | Four_oak | Full_house | Three_oak | Two_pair | One_pair | High_card
let int_of_hand_type : hand_type -> int =
  function
  | Five_oak -> 7
  | Four_oak -> 6
  | Full_house -> 5
  | Three_oak -> 4
  | Two_pair -> 3
  | One_pair -> 2
  | High_card -> 1
let compare_hand_types : hand_type -> hand_type -> int =
  fun h1 h2 ->
    compare (int_of_hand_type h1) (int_of_hand_type h2)
let string_of_hand_type : hand_type -> string =
  function
  | Five_oak -> "Five of a kind"
  | Four_oak -> "Four of a kind"
  | Full_house -> "Full house"
  | Three_oak -> "Three of a kind"
  | Two_pair -> "Two pair"
  | One_pair -> "One pair"
  | High_card -> "High card"

module Card = struct
  type t = | A | K | Q | J | T | Num of int
  let card_of_char : char -> t =
    function
    | 'A' -> A | 'K' -> K | 'Q' -> Q | 'J' -> J | 'T' -> T
    | '1' .. '9' as n -> 
      let n = String.make 1 n |> int_of_string in Num n
    | _ -> failwith "Unrecognized character"
  let int_of_card : t -> int =
    function
    | A -> 14
    | K -> 13
    | Q -> 12
    | T -> 10
    | Num n -> n
    | J -> 1
  let compare : t -> t -> int =
    fun c1 c2 ->
      compare (int_of_card c1) (int_of_card c2)
  let rec compare_seqs : t Seq.t -> t Seq.t -> int =
    fun s1 s2 ->
      match s1 (), s2 () with
      | Cons (c1, r1), Cons (c2, r2) -> 
        begin match compare c1 c2 with
        | 0 -> compare_seqs r1 r2
        | c -> c
        end
      | Nil, Nil -> 0
      | _ -> failwith "Unsupported comparison"
  let compare_arrays : t array -> t array -> int =
    fun arr1 arr2 ->
      compare_seqs (Array.to_seq arr1) (Array.to_seq arr2)
  let string_of_card : t -> string =
    function 
    | A -> "A" | K -> "K" | Q -> "Q" | J -> "J" | T -> "T" | Num n -> string_of_int n
end
module CardMap = Map.Make(Card)

let string_of_int_cardmap : int CardMap.t -> string =
  function card_map ->
    CardMap.to_seq card_map
    |> List.of_seq
    |> List.map (function c, n -> Printf.sprintf "%s: %d" (Card.string_of_card c) n)
    |> String.concat ", "

(* hand type, the actual cards, and the bid *)
type hand = hand_type * Card.t array * int
let compare_hands : hand -> hand -> int =
  fun (t1, arr1, _) (t2, arr2, _) ->
    match compare_hand_types t1 t2 with
    | 0 -> Card.compare_arrays arr1 arr2
    | c -> c
let string_of_hand : hand -> string =
  function hand_type, cards, bid ->
    let cards = Array.map Card.string_of_card cards |> Array.to_list |> String.concat "" in
    Printf.sprintf "%s %s %d" (string_of_hand_type hand_type) cards bid

type unclassified_hand = Card.t array * int
let string_of_unclassified_hand : unclassified_hand -> string =
  function cards, bid ->
    let cards = Array.map Card.string_of_card cards |> Array.to_list |> String.concat "" in
    Printf.sprintf "%s %d" cards bid

let hand_regex = Str.regexp {|\([AKQJT1-9]+\) \([0-9]+\)|}

let parse_cards : string -> Card.t array =
  fun s ->
    String.to_seq s |> Seq.map Card.card_of_char |> Array.of_seq

let parse_hand : string -> unclassified_hand =
  fun s ->
    if not (Str.string_match hand_regex s 0) then
      Printf.sprintf "Couldn't parse hand %s" s |> failwith else
    let cards = parse_cards (Str.matched_group 1 s) in
    let bid = Str.matched_group 2 s |> int_of_string in
    cards, bid

let parse_hands : string list -> unclassified_hand list =
  fun l -> List.map parse_hand l

let rec card_map_of_cards : Card.t list -> int CardMap.t =
  function 
  | [] -> CardMap.empty
  | c :: l ->
    let m = card_map_of_cards l in
    if CardMap.mem c m then
      CardMap.add c (CardMap.find c m + 1) m
    else
      CardMap.add c 1 m

let classify_hand_from_counts : int CardMap.t -> hand_type =
  function card_map ->
    Printf.printf "classify_hand_from_counts: %s\n" (string_of_int_cardmap card_map);
    match CardMap.cardinal card_map with
    | 1 -> Five_oak
    | 2 -> 
      if CardMap.exists (fun _ i -> i = 4) card_map then
        Four_oak
      else
        Full_house
    | 3 ->
      if CardMap.exists (fun _ i -> i = 3) card_map then
        Three_oak
      else
        Two_pair
    | 4 -> One_pair
    | 5 -> High_card
    | _ -> failwith "Unexpected number of card counts"

let classify_hand : unclassified_hand -> hand =
  (* use a map of counts to classify *)
  function cards, bid ->
    let card_map = Array.to_list cards |> card_map_of_cards in
    classify_hand_from_counts card_map, cards, bid

let most_common_card : int CardMap.t -> Card.t =
  function card_map ->
    CardMap.fold
      (fun card count so_far -> 
        let so_far_count = CardMap.find so_far card_map in
        if card = J then so_far 
        else if so_far = J then card
        else if count > so_far_count then
          card
        else if count = so_far_count && Card.int_of_card card > Card.int_of_card so_far then
          card
        else
          so_far)
      card_map
      ((function c, _ -> c) (CardMap.choose card_map))

let remove_jokers_from_card_map : int CardMap.t -> int CardMap.t =
  function card_map ->
    if CardMap.cardinal card_map = 1 && CardMap.mem J card_map then card_map else
    let common_card = most_common_card card_map in
    CardMap.add common_card (CardMap.find common_card card_map + CardMap.find J card_map) card_map
    |> CardMap.remove J

let classify_hand_with_jokers : unclassified_hand -> hand =
  (* construct a map of card counts
     if there are jokers present, modify them to be instances of the next most common card
     classify *)
  function cards, bid ->
    let card_map = Array.to_list cards |> card_map_of_cards in
    (* Printf.printf "classify_hand_with_jokers: %s\n" (string_of_int_cardmap card_map); *)
    let card_map = if CardMap.mem J card_map then remove_jokers_from_card_map card_map else card_map in
    classify_hand_from_counts card_map, cards, bid

let solve_part1 : string list -> string =
  fun lst ->
    (* sort hands in increasing strength and calculate scores *)
    parse_hands lst
    |> List.map classify_hand
    |> List.sort compare_hands
    (* |> List.map (fun h -> Printf.printf "%s\n" (string_of_hand h); h) *)
    |> List.mapi (fun i (_, _, bid) -> (i + 1) * bid) 
    |> List.fold_left (+) 0
    |> string_of_int


let solve_part2 : string list -> string =
  fun lst ->
    (* sort hands in increasing strength and calculate scores *)
    parse_hands lst
    |> List.map classify_hand_with_jokers
    (* |> List.map (fun h -> Printf.printf "%s\n" (string_of_hand h); h) *)
    |> List.sort compare_hands
    |> List.mapi (fun i (_, _, bid) -> (i + 1) * bid) 
    |> List.fold_left (+) 0
    |> string_of_int

let solve : string list -> string =
  fun lst ->
    solve_part2 lst