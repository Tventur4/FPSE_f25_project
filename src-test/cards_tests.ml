(*
  Testing file for card-based modules: Card, Card_set, and Deck.
*)

open Core
open OUnit2

let deck = Deck.sorted_deck

let seven_card_list1 = 
  [ {Card.suit = Hearts ; rank = Jack}
  ; {Card.suit = Diamonds ; rank = Jack}
  ; {Card.suit = Spades ; rank = Jack}
  ; {Card.suit = Clubs ; rank = Three}
  ; {Card.suit = Hearts ; rank = Three}
  ; {Card.suit = Diamonds ; rank = Seven}
  ; {Card.suit = Spades ; rank = Queen}]

let seven_card_list2 = 
  [ {Card.suit = Hearts ; rank = Jack}
  ; {Card.suit = Diamonds ; rank = Jack}
  ; {Card.suit = Spades ; rank = Two}
  ; {Card.suit = Clubs ; rank = Four}
  ; {Card.suit = Hearts ; rank = Six}
  ; {Card.suit = Diamonds ; rank = Eight}
  ; {Card.suit = Spades ; rank = Queen}]

let seven_card_list3 = 
  [ {Card.suit = Spades ; rank = Jack}
  ; {Card.suit = Spades ; rank = Ten}
  ; {Card.suit = Spades ; rank = Nine}
  ; {Card.suit = Spades ; rank = Eight}
  ; {Card.suit = Spades ; rank = Seven}
  ; {Card.suit = Hearts ; rank = Seven}
  ; {Card.suit = Clubs ; rank = Seven}]

let first_five_cards =
  [ Card.int_to_card 0
  ; Card.int_to_card 1
  ; Card.int_to_card 2
  ; Card.int_to_card 3
  ; Card.int_to_card 4]

let hand1 = Card_set.of_7_cards seven_card_list1

let hand2 = Card_set.of_7_cards seven_card_list2

let hand3 = Card_set.of_7_cards seven_card_list3

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

let test_card_to_string _ =
  assert_equal "TwoClubs" @@ Card.to_string (Card.int_to_card 0);
  assert_equal "AceSpades" @@ Card.to_string (Card.int_to_card 51)

let test_deck_num_cards _ =
  let rec correct_size_deck (index : int) (deck : Deck.t) : bool =
    let rest = Deck.burn_card deck in
    match index with 
    | 51 -> Deck.num_cards deck = 1
    | x -> (Deck.num_cards deck = (52 - x)) && (correct_size_deck (index + 1) rest)
  in
  assert_equal true @@ correct_size_deck 0 deck

let test_deck_draw_cards _ =
  assert_equal (Card.int_to_card 0) @@ fst (Deck.draw_card deck);
  assert_equal first_five_cards @@ fst (Deck.draw_cards deck 5)

let test_of_7_cards _ =
  assert_equal 6 @@ Card_set.value_of_hand hand1;
  assert_equal 1 @@ Card_set.value_of_hand hand2;
  assert_equal 8 @@ Card_set.value_of_hand hand3

let test_compare_hands _ =
  assert_equal true @@ (Card_set.compare hand1 hand2 > 0);
  assert_equal true @@ (Card_set.compare hand1 hand3 < 0);
  assert_equal true @@ (Card_set.compare hand2 hand3 < 0)

let series =
  "cards_tests" >:::
  [ "card_to_int" >:: test_card_to_int
  ; "int_to_card" >:: test_int_to_card
  ; "card_to_string" >:: test_card_to_string
  ; "deck_num_cards" >:: test_deck_num_cards
  ; "deck_draw_cards" >:: test_deck_draw_cards
  ; "of_7_cards" >:: test_of_7_cards
  ; "compare_hands" >:: test_compare_hands]
