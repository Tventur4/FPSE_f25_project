(*
  The Card module defines a concrete type for playing cards as well as functions to construct, print, and convert them.
  The type holds a suit and a rank, both of which are variants defined below.
*)

type suit = Clubs | Diamonds | Hearts | Spades [@@deriving sexp, compare, equal, variants, enumerate]
(** [suit] is a comparable, serializable type to represent the suit attribute of a playing card. 
    Comparison is alphabetical on the string representation of the suit. *)

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace [@@deriving sexp, compare, equal]
(** [rank] is a comparable, serializable type to represent the rank attribute of a playing card. 
    Comparison is ascending on the rank-value with respect to poker. Note that this is why the Ace is valued higher 
    than every other rank. *)

type action = Fold | Check | Call | Bet of Chips.t | Raise of Chips.t [@@deriving sexp]

type betting_round = PreFlop | Flop | Turn | River | Showdown [@@deriving sexp, equal]

type t =
  { suit : suit
  ; rank : rank} [@@deriving sexp, compare, equal]
(** [t] is a comparable, serializable type to represent a playing card.
    It is a record containing two fields of types suit and rank, respectively. *)

val suit_to_int : suit -> int
(** [suit_to_int s] is the numeric representation of [s], with [Clubs] -> 0, [Diamonds] -> 1, [Hearts] -> 2, 
    and [Spades] -> 3. *)

val rank_to_int : rank -> int
(** [rank_to_int r] is the numeric representation of [r], where each rank is assigned a value from 0-12 corresponding 
    to their rank-value with respect to poker (i.e. [rank_to_int 2] = 0 and [rank_to_int Ace] = 12). *)

val card_to_int : t -> int
(** [card_to_int card] is the numeric representation of [card], where the card is assigned a value from 0-51
    corresponding to its index in a 52-card deck sorted in ascending order by rank and then by suit. *)

val int_to_card : int -> t
(** [int_to_card index] converts an index (i.e. a number between 0 and 51 inclusive) into a card with appropriate 
    suit and index. This is an inverse function to [card_to_int]. *)

val to_string : t -> string
(** [to_string card] gets the string representation of [card], with the format of "[rank] of [suit]." *)





