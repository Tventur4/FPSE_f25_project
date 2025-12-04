open Core

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
