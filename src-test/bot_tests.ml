(*
  Testing file for Bot modules.
*)
open Core
open OUnit2

let bot1 = { Bot.diff = Easy ; bot_type = Always_fold }
let bot2 = { Bot.diff = Easy ; bot_type = All_in }
let bot3 = { Bot.diff = Easy ; bot_type = Rule_hand_only }
let bot4 = { Bot.diff = Medium ; bot_type = Rule_best_hand }
let bot5 = { Bot.diff = Hard ; bot_type = Rule_hand_only }
let bot6 = { Bot.diff = Expert ; bot_type = Rule_best_hand }

let hole_cards1 = { Card.suit = Clubs ; rank = Two }, { Card.suit = Spades ; rank = Seven }
let hole_cards2 = { Card.suit = Clubs ; rank = Two }, { Card.suit = Spades ; rank = Two }
let hole_cards3 = { Card.suit = Hearts ; rank = Ace }, { Card.suit = Hearts ; rank = Jack }
let hole_cards4 = { Card.suit = Clubs ; rank = Ace }, { Card.suit = Spades ; rank = Ace }

let com_cards1 = 
  [ { Card.suit = Diamonds ; rank = Jack }
  ; { Card.suit = Spades ; rank = Jack }
  ; { Card.suit = Clubs ; rank = Jack }]
let com_cards2 = 
  [ { Card.suit = Diamonds ; rank = Two }
  ; { Card.suit = Spades ; rank = Four }
  ; { Card.suit = Clubs ; rank = Jack }]
let com_cards3 = 
  [ { Card.suit = Diamonds ; rank = Jack }
  ; { Card.suit = Spades ; rank = Queen }
  ; { Card.suit = Clubs ; rank = King }]
let com_cards4 = 
  [ { Card.suit = Diamonds ; rank = Four }
  ; { Card.suit = Spades ; rank = Five }
  ; { Card.suit = Clubs ; rank = King }]
let com_cards5 = 
  [ { Card.suit = Diamonds ; rank = Jack }
  ; { Card.suit = Spades ; rank = Jack }
  ; { Card.suit = Clubs ; rank = Jack }
  ; { Card.suit = Clubs ; rank = Two }]
let com_cards6 = 
  [ { Card.suit = Diamonds ; rank = Jack }
  ; { Card.suit = Spades ; rank = Jack }
  ; { Card.suit = Clubs ; rank = Jack }
  ; { Card.suit = Clubs ; rank = Two }
  ; { Card.suit = Spades ; rank = Seven }]

let test_fold_bot _ =
  assert_equal Card.Fold @@ Bot.make_move bot1 PreFlop (Chips.of_int 100) [] 2 hole_cards1 (Chips.of_int 1000)

let test_all_in_bot _ =
  assert_equal (Card.Bet (Chips.of_int 1000)) @@ Bot.make_move bot2 PreFlop Chips.zero [] 2 hole_cards1 (Chips.of_int 1000);
  assert_equal (Card.Raise (Chips.of_int 500)) @@ Bot.make_move bot2 PreFlop (Chips.of_int 500) [] 2 hole_cards1 (Chips.of_int 1000);
  assert_equal Card.Call @@ Bot.make_move bot2 PreFlop (Chips.of_int 1250) [] 2 hole_cards1 (Chips.of_int 1000)

let test_hand_only_bot_bracket_preflop _ =
  assert_equal 0 @@ Bot_hand_only.get_bracket_hand_only 0 PreFlop [] hole_cards1;
  assert_equal 1 @@ Bot_hand_only.get_bracket_hand_only 0 PreFlop [] hole_cards2;
  assert_equal 2 @@ Bot_hand_only.get_bracket_hand_only 0 PreFlop [] hole_cards3;
  assert_equal 3 @@ Bot_hand_only.get_bracket_hand_only 0 PreFlop [] hole_cards4

let test_hand_only_bot_bracket_flop _ =
  assert_equal 3 @@ Bot_hand_only.get_bracket_hand_only 0 Flop com_cards1 hole_cards4;
  assert_equal 2 @@ Bot_hand_only.get_bracket_hand_only 0 Flop com_cards2 hole_cards2;
  assert_equal 1 @@ Bot_hand_only.get_bracket_hand_only 0 Flop com_cards3 hole_cards2;
  assert_equal 0 @@ Bot_hand_only.get_bracket_hand_only 0 Turn com_cards3 hole_cards2;
  assert_equal 0 @@ Bot_hand_only.get_bracket_hand_only 0 Flop com_cards4 hole_cards1

let test_hand_only_bot_make_move _ =
  assert_equal Card.Fold @@ Bot.make_move bot3 Flop Chips.zero com_cards4 2 hole_cards1 (Chips.of_int 1000);
  assert_equal Card.Check @@ Bot.make_move bot3 Flop Chips.zero com_cards3 2 hole_cards2 (Chips.of_int 1000);
  assert_equal Card.Fold @@ Bot.make_move bot3 Flop (Chips.of_int 1001) com_cards2 2 hole_cards2 (Chips.of_int 1000);
  assert_equal Card.Call @@ Bot.make_move bot3 Flop (Chips.of_int 200) com_cards2 2 hole_cards2 (Chips.of_int 1000);
  assert_equal Card.Fold @@ Bot.make_move bot3 Flop (Chips.of_int 300) com_cards2 2 hole_cards2 (Chips.of_int 1000);
  assert_equal (Card.Bet (Chips.of_int 100)) @@ Bot.make_move bot3 PreFlop Chips.zero [] 2 hole_cards4 (Chips.of_int 1000);
  assert_equal (Card.Bet (Chips.of_int 125)) @@ Bot.make_move bot5 Flop Chips.zero com_cards1 2 hole_cards4 (Chips.of_int 1000);
  assert_equal (Card.Raise (Chips.of_int 100)) @@ Bot.make_move bot3 Turn (Chips.of_int 500) com_cards5 2 hole_cards4 (Chips.of_int 1000);
  assert_equal (Card.Raise (Chips.of_int 250)) @@ Bot.make_move bot3 River (Chips.of_int 500) com_cards6 2 hole_cards4 (Chips.of_int 1000);
  assert_equal Card.Call @@ Bot.make_move bot3 Flop (Chips.of_int 100) com_cards1 2 hole_cards4 (Chips.of_int 1000);
  assert_equal Card.Fold @@ Bot.make_move bot3 Flop (Chips.of_int 350) com_cards1 2 hole_cards4 (Chips.of_int 1000)

let test_best_hand_bot_bracket _ =
  assert_equal 3 @@ Bot_best_hand.get_bracket_best_hand 1 River com_cards6 2 hole_cards3;
  assert_equal 0 @@ Bot_best_hand.get_bracket_best_hand 3 PreFlop [] 2 hole_cards1

let test_best_hand_bot_make_move _ =
  assert_equal (Card.Bet (Chips.of_int 500)) @@ Bot.make_move bot4 River Chips.zero com_cards6 2 hole_cards3 (Chips.of_int 1000);
  assert_equal Card.Fold @@ Bot.make_move bot6 PreFlop Chips.zero [] 2 hole_cards1 (Chips.of_int 1000)

let series =
  "bot_tests" >:::
  [ "fold_bot" >:: test_fold_bot
  ; "all_in_bot" >:: test_all_in_bot
  ; "hand_only_bot_bracket_preflop" >:: test_hand_only_bot_bracket_preflop
  ; "hand_only_bot_bracket_flop" >:: test_hand_only_bot_bracket_flop
  ; "hand_only_bot_make_move" >:: test_hand_only_bot_make_move]