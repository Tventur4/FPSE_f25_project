open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

(*
  Placeholder for unimplemented functions.
*)
let unimplemented () =
  failwith "unimplemented"

module C = Card

type t = C.t list [@@deriving sexp]

(*
  [get_deck] is a standard deck of 52 playing cards sorted in ascending order by rank and then by suit.

  @return A standard deck.
*)
let get_deck () : t =
  unimplemented ()

(*
  [shuffle deck] shuffles [deck], randomly rearranging the card.
  
  @param deck The deck to shuffle.
  @return The shuffled deck.
*)
let shuffle (deck : t) : t =
  unimplemented ()

(*
  [num_card deck] is the number of cards currently in [deck].

  @param deck The deck.
  @return The number of cards currently in [deck].
*)
let num_cards (deck : t) : int =
  unimplemented ()

(*
  [draw_card deck] draw a single card from the top of [deck].
  
  @param deck The deck from which to draw a card.
  @return The card at the "top".
  @throws failwith if there are no cards left in [deck].
*)
let draw_card (deck : t) : C.t =
  unimplemented ()

(*
  [draw_cards deck n] draw [n] cards from the top of [deck]
  
  @param The deck from which to draw a card.
  @param n The number of cards to draw.
  @return The first [n] cards drawn from the top.
  @throws failwith if there are less than [n] cards remaining in the deck.
*)
let draw_cards (deck : t) (n : int) : C.t list =
  unimplemented ()

(*
  [burn_card deck] takes a card from the top of [deck] and discards it.

  @param deck The deck from which to burn a card.
  @throws failwith if there are no cards left in [deck].
*)
let burn_card (deck : t) : unit =
  unimplemented ()

