(*
  Testing file for card-based modules: Card, Card_set, and Deck.
*)

open Core
open OUnit2

let deck = Deck.sorted_deck

let test_card_to_int _ =
  let rec card_matches_index (index : int) (deck : Deck.t) : bool =
    let card, rest = Deck.draw_card deck in
    match index with
    | 51 -> Card.card_to_int card = 51
    | x -> (Card.card_to_int card = x) && (card_matches_index (index + 1) rest)
  in
  assert_equal true @@ card_matches_index 0 deck

let test_int_to_card _ =
  let rec index_matches_card (index : int) : bool =
    match index with
    | 51 -> (Card.card_to_int (Card.int_to_card 51)) = 51
    | x -> (Card.card_to_int (Card.int_to_card x)) = x && (index_matches_card (index + 1))
  in
  assert_equal true @@ index_matches_card 0

let test_deck_num_cards _ =
  let rec correct_size_deck (index : int) (deck : Deck.t) : bool =
    let rest = Deck.burn_card deck in
    match index with 
    | 51 -> Deck.num_cards deck = 1
    | x -> (Deck.num_cards deck = (52 - x)) && (correct_size_deck (index + 1) rest)
  in
  assert_equal true @@ correct_size_deck 0 deck

let series =
  "cards_tests" >:::
  [ "card_to_int" >:: test_card_to_int
  ; "int_to_card" >:: test_int_to_card
  ; "deck_num_cards" >:: test_deck_num_cards]
