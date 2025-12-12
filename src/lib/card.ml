open Core

type suit = Clubs | Diamonds | Hearts | Spades [@@deriving sexp, compare, equal, variants, enumerate]
(** [suit] is a comparable, serializable type to represent the suit attribute of a playing card. 
    Comparison is alphabetical on the string representation of the suit. *)

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace [@@deriving sexp, compare, equal]
(** [rank] is a comparable, serializable type to represent the rank attribute of a playing card. 
    Comparison is ascending on the rank-value with respect to poker. Note that this is why the Ace is valued higher 
    than every other rank. *)

type action = Fold | Check | Call | Bet of int | Raise of int [@@deriving sexp]
    
type betting_round = PreFlop | Flop | Turn | River | Showdown [@@deriving sexp, equal]

type t =
  { suit : suit
  ; rank : rank} [@@deriving sexp, compare, equal]
(** [t] is a comparable, serializable type to represent a playing card.
      It is a record containing two fields of types suit and rank, respectively. *)

(*TODO: this function can be derived with [@@deriving variants]*)
let suit_to_int (s : suit) : int =
  match s with
   | Clubs -> 0
   | Diamonds -> 1
   | Hearts -> 2
   | Spades -> 3

let rank_to_int (r : rank) : int =
  match r with
  | Two -> 0
  | Three -> 1
  | Four -> 2
  | Five -> 3
  | Six -> 4
  | Seven -> 5
  | Eight -> 6
  | Nine -> 7
  | Ten -> 8
  | Jack -> 9
  | Queen -> 10
  | King -> 11
  | Ace -> 12

let card_to_int (card : t) : int =
  suit_to_int(card.suit) * 13 + rank_to_int(card.rank)

(*TODO: You can use [@@deriving enumerate] from ppx_enumerate, then convert that list to an array and index the array. So most functionality in this module is derivable with ppx.*)
  let int_to_suit (n : int) : suit =
  match n with
  | 1 -> Diamonds
  | 2 -> Hearts
  | 3 -> Spades
  | _ -> Clubs

let int_to_rank (n : int) : rank =
  match n with
  | 1 -> Three
  | 2 -> Four
  | 3 -> Five
  | 4 -> Six
  | 5 -> Seven
  | 6 -> Eight
  | 7 -> Nine
  | 8 -> Ten
  | 9 -> Jack
  | 10 -> Queen
  | 11 -> King
  | 12 -> Ace
  | _ -> Two 

let int_to_card (n : int) : t = 
  { suit = int_to_suit(n / 13) ; rank = int_to_rank(n mod 13)}

let to_string (card : t) : string =
  Sexp.to_string(sexp_of_rank(card.rank)) ^ Sexp.to_string(sexp_of_suit(card.suit))