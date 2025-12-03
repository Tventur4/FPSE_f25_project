open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

type t = Card.t list [@@deriving sexp]

(*
  [get_deck] is a standard deck of 52 playing cards sorted in ascending order by rank and then by suit.
  2-A of clubs then diamonds...

  @return A standard deck.
*)
let get_deck () : t =
  let suits = [Card.Spades; Card.Hearts; Card.Diamonds; Card.Clubs] in
  let ranks =
    [ Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven
    ; Card.Eight; Card.Nine; Card.Ten; Card.Jack; Card.Queen; Card.King
    ; Card.Ace ]
  in
  List.concat_map suits ~f:(fun suit ->
      List.map ranks ~f:(fun rank -> {Card.suit; rank}))

(*
  [shuffle deck] shuffles [deck], randomly rearranging the card.
  
  @param deck The deck to shuffle.
  @return The shuffled deck.
*)
let shuffle (deck : t) : t =
  List.permute deck

(*
  [num_card deck] is the number of cards currently in [deck].

  @param deck The deck.
  @return The number of cards currently in [deck].
*)
let num_cards (deck : t) : int =
  List.length deck

(*
  [draw_card deck] draw a single card from the top of [deck].
  
  @param deck The deck from which to draw a card.
  @return The card at the "top".
  @throws failwith if there are no cards left in [deck].
*)
let draw_card (deck : t) : Card.t =
  match deck with
  | [] -> failwith "No cards left in deck"
  | card :: _ -> card

(*
  [draw_cards deck n] draw [n] cards from the top of [deck]
  
  @param The deck from which to draw a card.
  @param n The number of cards to draw.
  @return The first [n] cards drawn from the top.
  @throws failwith if there are less than [n] cards remaining in the deck.
*)
let draw_cards (deck : t) (n : int) : Card.t list =
  if List.length deck < n then
    failwith "Not enough cards left in deck"
  else
    List.take deck n

(*
  [burn_card deck] takes a card from the top of [deck] and discards it.

  @param deck The deck from which to burn a card.
  @throws failwith if there are no cards left in [deck].
*)
let burn_card (deck : t) : unit =
  draw_card deck |> ignore;


