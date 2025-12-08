(*
  The Deck module defines a concrete type for a standard deck of 52 playing cards as well as functions to construct, 
  manipulate, and print them.
*)


type t (*= C.t list [@@deriving sexp] *)

(** [t] is a serializable type to represent a deck of playing cards.
    Note that the "top" of the deck refers to the card at index 0. *)

val sorted_deck : t
(** [sorted_deck] is a standard deck of 52 playing cards sorted in ascending order by rank and then by suit. *)

val shuffle : t -> t
(** [shuffle deck] shuffles [deck], randomly rearranging the card. *)

val num_cards : t -> int
(** [num_card deck] is the number of cards currently in [deck]. *)

val draw_card : t -> Card.t * t
(** [draw_card deck] draws a single card from the top of [deck]. *)

val draw_cards : t -> int -> Card.t list 
(** [draw_cards deck n] draw [n] cards from the top of [deck]. *)

val burn_card : t -> t
(** [burn_card deck] takes a card from the top of [deck] and discards it. *)


