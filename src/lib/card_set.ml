open Core

[@@@ocaml.warning "-27"]

type hand_category =
  | HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
[@@deriving sexp, compare, equal]

type t = {
  category : hand_category;
  cards : Card.t list (*5 cards that make up a hand*);
  tiebreakers : int list (* rank integers used for breaking a tie*)
}
[@@deriving sexp]

(*compare function for cards based on rank value (descending)*)
let compare_card_desc (c1 : Card.t) (c2 : Card.t) : int =
  Int.compare (Card.rank_to_int c2.rank) (Card.rank_to_int c1.rank)

(* for identifying pairs/full houses*)
let group_by_rank (cards : Card.t list) : (int * int) list = 
  let counts = 
    List.fold cards ~init:Int.Map.empty ~f:(fun acc card -> 
      let r = Card.rank_to_int card.rank in
      Map.update acc r ~f:(function
        | None -> 1
        | Some x -> x + 1)
      ) in
      Map.to_alist counts
      |> List.map ~f:(fun (r, count) -> (count, r))
      (*TODO: Tuple2.compare*)
      |> List.sort ~compare:(fun (c1, r1) (c2, r2) -> 
        match Int.compare c2 c1 with
        | 0 -> Int.compare r2 r1
        | x -> x)

let get_straight_high_rank (cards : Card.t list) : int option = 
  let ranks = List.map cards ~f:(fun c -> Card.rank_to_int c.rank) in
  match ranks with
  | [a; b; c; d; e] when a = b + 1 && b = c + 1 && c = d + 1 && d = e + 1 -> Some a 
  | [ 12; 3; 2; 1; 0] -> Some 3
  | _ -> None

let is_flush (cards : Card.t list) : bool = 
  match cards with
  | [] -> false
  | c :: rest -> List.for_all rest ~f:(fun x -> Card.suit_to_int x.suit = Card.suit_to_int c.suit)

let evaluate (cards : Card.t list) : t =
  (* edge case but shouldn't pop up*) 
  if List.length cards <> 5 then failwith "Hand Evaluation needs 5 cards";

  let sorted = List.sort cards ~compare: compare_card_desc in 
  let flush = is_flush sorted in
  let straight = get_straight_high_rank sorted in
  let groups = group_by_rank sorted in

  (*time to match with all possible hands*)
  match (flush, straight, groups) with
  | (true, Some 12, _) -> {category = RoyalFlush; cards = sorted; tiebreakers = []}
  | (true, Some h, _) -> {category = StraightFlush; cards = sorted; tiebreakers = [h]}
  (*if no Royal or Straight flush, analyze the remaning groups*)
  | (_, _, (4, r1) :: (1, r2) :: _) ->
    {category = FourOfAKind; cards = sorted; tiebreakers = [r1; r2]}
  | (_, _, (3, r1) :: (2, r2) :: _) ->
    {category = FullHouse; cards = sorted; tiebreakers = [r1; r2]}
  | (true, None, _) ->
    {category = Flush; cards = sorted; tiebreakers = List.map sorted ~f:(fun c -> Card.rank_to_int c.rank)}
  | (false, Some h, _) ->
    {category = Straight; cards = sorted; tiebreakers = [h]}
  | (_, _, (3, r1) :: rest) ->
    {category = ThreeOfAKind; cards = sorted; tiebreakers = r1 :: List.map rest ~f:snd}
  | (_, _, (2, r1) :: rest) ->
    {category = Pair; cards = sorted; tiebreakers = r1 :: List.map rest ~f:snd}
  | _ -> 
    {category = HighCard; cards = sorted; tiebreakers = List.map sorted ~f:(fun c -> Card.rank_to_int c.rank)}

let compare (h1 : t) (h2 : t) : int = 
  let cat_comparison = compare_hand_category h1.category h2.category in
  if cat_comparison <> 0 then cat_comparison (*if hand category is enough to determine winner of hands use that*)
  else List.compare Int.compare h1.tiebreakers h2.tiebreakers (*else use the tiebreakers*)

let rec choose_sublists (k : int) (list : 'a list) : 'a list list =
  match k, list with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | k, hd :: tl ->
    let with_hd =
      List.map ~f:(fun tl' -> hd :: tl') (choose_sublists (k - 1) tl)
    in
    let without_hd = choose_sublists k tl in
    with_hd @ without_hd

let handle_option (o : 'a option) =
  match o with
  | Some v -> v
  | None -> failwith "value not found"

let of_7_cards (seven_cards : Card.t list) : t =
  match seven_cards with
  | [c1; c2; c3; c4; c5; c6; c7] ->
    let fives = choose_sublists 5 seven_cards in
    let evaluated =
      List.map ~f:evaluate fives
    in
    List.fold ~f:(fun best h -> if compare h best > 0 then h else best) ~init:(handle_option (List.hd evaluated)) (handle_option (List.tl evaluated))
  | _ ->
    invalid_arg "of_7_cards requires exactly 7 cards"

let value_of_hand (hand : t) : int = 
  (*TODO: Derive this with ppx_variants_conv*)
  match hand.category with
  | HighCard -> 0
  | Pair -> 1
  | TwoPair -> 2
  | ThreeOfAKind -> 3
  | Straight -> 4
  | Flush -> 5
  | FullHouse -> 6
  | FourOfAKind -> 7
  | StraightFlush -> 8
  | RoyalFlush -> 9

let to_string (hand : t) : string =
  match hand.category with
  | RoyalFlush -> "Royal Flush"
  | StraightFlush -> "Straight Flush"
  | FourOfAKind -> "Four of a Kind"
  | FullHouse -> "Full House"
  | Flush -> "Flush"
  | Straight -> "Straight"
  | ThreeOfAKind -> "Three of a Kind"
  | TwoPair -> "Two Pair"
  | Pair -> "Pair"
  | HighCard -> "High Card"