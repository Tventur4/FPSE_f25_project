open Core
open OUnit2

let bot1 = { Bot.diff = Easy ; bot_type = Always_fold }
let bot2 = { Bot.diff = Easy ; bot_type = All_in }
let bot3 = { Bot.diff = Easy ; bot_type = Rule_hand_only }
let bot4 = { Bot.diff = Medium ; bot_type = Rule_best_hand }
let bot5 = { Bot.diff = Hard ; bot_type = Rule_hand_only }
let bot6 = { Bot.diff = Expert ; bot_type = Rule_best_hand }

let hole_cards1 = { Card.suit = Clubs ; rank = Two }, {Card.suit = Spades ; rank = Seven}

let test_fold_bot _ =
  assert_equal Card.Fold @@ Bot.make_move bot1 PreFlop 100 [] 2 hole_cards1 1000

let test_all_in_bot _ =
  assert_equal (Card.Bet 1000) @@ Bot.make_move bot2 PreFlop 0 [] 2 hole_cards1 1000;
  assert_equal (Card.Raise 500) @@ Bot.make_move bot2 PreFlop 500 [] 2 hole_cards1 1000;
  assert_equal Card.Call @@ Bot.make_move bot2 PreFlop 1250 [] 2 hole_cards1 1000

let series =
  "bot_tests" >:::
  [ "fold_bot" >:: test_fold_bot
  ; "all_in_bot" >:: test_all_in_bot]