open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

(*
  Placeholder for unimplemented functions.
*)
let unimplemented () =
  failwith "unimplemented"

type suit = Clubs | Diamonds | Hearts | Spades [@@deriving sexp, compare, equal]

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace [@@deriving sexp, compare, equal]

type t = { suit : suit; rank : rank} [@@deriving sexp, compare, equal]

(*
  [suit_to_int s] is the numeric representation of [s], with [Clubs] -> 0, [Diamonds] -> 1, [Hearts] -> 2, 
  and [Spades] -> 3.

  @param s Suit.
  @return The numeric representation of [s].
*)
let suit_to_int (s : suit) : int =
  unimplemented ()

(*
  [rank_to_int r] is the numeric representation of [r], where each rank is assigned a value from 0-12 corresponding 
  to their rank-value with respect to poker (i.e. [rank_to_int 2] = 0 and [rank_to_int Ace] = 12).

  @param r Rank.
  @return The numeric representation of [r].
*)
let rank_to_int (r : rank) : int =
  unimplemented ()

(*
  [card_to_int card] is the numeric representation of [card], where the card is assigned a value from 0-51
  corresponding to its index in a 52-card deck sorted in ascending order by rank and then by suit.

  @param card Input card to convert.
  @return The numeric representation of [card].
*)
let card_to_int (card : t) : int =
  unimplemented ()

(*
  [int_to_card index] converts an index (i.e. a number between 0 and 51 inclusive) into a card with appropriate 
  suit and index. This is an inverse function to [card_to_int].

  @param index The integer index of the desired card.
  @return The card correspdoning to [index].
  @throws invalid_arg if [index] < 0 or [index] > 51.
*)
let int_to_card (index : int) : t =
  unimplemented ()

(*
  [to_string card] gets the string representation of [card], with the format of "[rank] of [suit]."
  
  @param card The input card.
  @return The string representation of [card].
*)
let to_string (card : t) : string =
  unimplemented ()




