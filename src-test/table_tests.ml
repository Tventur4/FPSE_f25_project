(*
  Testing file for the player and table modules.
*)

open Core
open OUnit2

let bot_1 = {Bot.diff = Easy ; bot_type = All_in}
let bot_2 = {Bot.diff = Easy ; bot_type = Always_fold}

let player_tobi = Player.make_player "Tobi" 0 None 100
let player_vrinda = Player.make_player "Vrinda" 1 None 100
let player_timmy = Player.make_player "Timmy" 2 None 100
let player_super_evil_bot = Player.make_player "Super Evil Bot" 3 (Some bot_1) 100
let player_broke_bot = Player.make_player "Very Broke Bot" 4 (Some bot_2) 0

let players = [player_tobi ; player_vrinda ; player_timmy ; player_super_evil_bot]
let players_modified = [player_tobi ; player_vrinda ; player_timmy ; player_super_evil_bot ; player_broke_bot]

let table = Table.init players
let table_modified = Table.init players_modified

let test_player_add_chips _ =
  assert_equal 110 @@ (Player.add_chips player_tobi 10).chip_stack;
  assert_equal 125 @@ (Player.add_chips (Player.add_chips player_vrinda 15) 10).chip_stack;
  assert_equal 100 @@ (Player.add_chips player_timmy 0).chip_stack

let test_player_remove_chips _ =
  assert_equal 90 @@ (Player.remove_chips player_tobi 10).chip_stack;
  assert_equal 75 @@ (Player.remove_chips (Player.remove_chips player_vrinda 15) 10).chip_stack;
  assert_equal 0 @@ (Player.remove_chips player_timmy 100).chip_stack

let test_player_set_hole_cards _ =
  let hole_cards_1 = ({Card.suit = Clubs ; rank = Two}, {Card.suit = Diamonds ; rank = Seven}) in
  assert_equal None @@ player_tobi.hole_cards;
  assert_equal (Some hole_cards_1) @@ (Player.set_hole_cards player_tobi hole_cards_1).hole_cards

let test_table_init _ =
  assert_equal players @@ Table.current_players table;
  assert_equal players @@ Table.get_active_players table;
  assert_equal player_tobi @@ Table.get_player_at_turn table

let test_table_advance_turn _ =
  let table_1 = Table.advance_turn table in
  assert_equal player_vrinda @@ Table.get_player_at_turn table_1;
  let table_2 = Table.advance_turn table_1 in
  assert_equal player_timmy @@ Table.get_player_at_turn table_2;
  let table_3 = Table.advance_turn table_2 in
  assert_equal player_super_evil_bot @@ Table.get_player_at_turn table_3;
  let table_4 = Table.advance_turn table_3 in
  assert_equal player_tobi @@ Table.get_player_at_turn table_4

let test_table_inactive_player _ =
  assert_equal players @@ Table.get_active_players table_modified;
  let table_temp = (Table.advance_turn (Table.advance_turn (Table.advance_turn table))) in
  assert_equal player_super_evil_bot @@ Table.get_player_at_turn table_temp;
  assert_equal player_tobi @@ Table.get_player_at_turn (Table.advance_turn table_temp)

let series =
  "table_tests" >:::
  [ "player_add_chips" >:: test_player_add_chips
  ; "player_remove_chips" >:: test_player_remove_chips
  ; "player_set_hole_cards" >:: test_player_set_hole_cards
  ; "table_init" >:: test_table_init
  ; "table_advance_turn" >:: test_table_advance_turn
  ; "table_inactive_player" >:: test_table_inactive_player]

