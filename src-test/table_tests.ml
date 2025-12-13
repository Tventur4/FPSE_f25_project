(*
  Testing file for the Player and Table modules.
*)

open Core
open OUnit2

let bot1 = {Bot.diff = Easy ; bot_type = All_in}
let bot2 = {Bot.diff = Easy ; bot_type = Always_fold}

let player_tobi = Player.make_player "Tobi" 0 None (Chips.of_int 100)
let player_vrinda = Player.make_player "Vrinda" 1 None (Chips.of_int 100)
let player_timmy = Player.make_player "Timmy" 2 None (Chips.of_int 100)
let player_super_evil_bot = Player.make_player "Super Evil Bot" 3 (Some bot1) (Chips.of_int 100)
let player_broke_bot = Player.make_player "Very Broke Bot" 4 (Some bot2) Chips.zero

let players = [player_tobi ; player_vrinda ; player_timmy ; player_super_evil_bot]
let players_modified = [player_tobi ; player_vrinda ; player_timmy ; player_super_evil_bot ; player_broke_bot]

let table = Table.init players
let table_modified = Table.init players_modified

let test_player_add_chips _ =
  assert_equal (Chips.of_int 110) @@ (Player.add_chips player_tobi (Chips.of_int 10)).chip_stack;
  assert_equal (Chips.of_int 125) @@ (Player.add_chips (Player.add_chips player_vrinda (Chips.of_int 15)) (Chips.of_int 10)).chip_stack;
  assert_equal (Chips.of_int 100) @@ (Player.add_chips player_timmy Chips.zero).chip_stack

let test_player_remove_chips _ =
  (match Player.remove_chips player_tobi (Chips.of_int 10) with
  | Ok p -> assert_equal (Chips.of_int 90) p.chip_stack
  | Error _ -> assert_failure "Should be able to remove 10 chips");
  (match Player.remove_chips player_vrinda (Chips.of_int 15) with
  | Ok p1 -> 
    (match Player.remove_chips p1 (Chips.of_int 10) with
    | Ok p2 -> assert_equal (Chips.of_int 75) p2.chip_stack
    | Error _ -> assert_failure "Should be able to remove 10 chips")
  | Error _ -> assert_failure "Should be able to remove 15 chips");
  (match Player.remove_chips player_timmy (Chips.of_int 100) with
  | Ok p -> assert_equal Chips.zero p.chip_stack
  | Error _ -> assert_failure "Should be able to remove all chips")

let test_player_set_hole_cards _ =
  let hole_cards_1 = ({Card.suit = Clubs ; rank = Two}, {Card.suit = Diamonds ; rank = Seven}) in
  assert_equal None @@ player_tobi.hole_cards;
  assert_equal (Some hole_cards_1) @@ (Player.set_hole_cards player_tobi hole_cards_1).hole_cards

let test_table_init _ =
  assert_equal players @@ Table.current_players table;
  assert_equal players @@ Table.get_active_players table;
  assert_equal player_tobi @@ Table.get_player_at_turn table

let test_table_advance_turn _ =
  let table1 = Table.advance_turn table in
  assert_equal player_vrinda @@ Table.get_player_at_turn table1;
  let table2 = Table.advance_turn table1 in
  assert_equal player_timmy @@ Table.get_player_at_turn table2;
  let table3 = Table.advance_turn table2 in
  assert_equal player_super_evil_bot @@ Table.get_player_at_turn table3;
  let table4 = Table.advance_turn table3 in
  assert_equal player_tobi @@ Table.get_player_at_turn table4

let test_table_inactive_player _ =
  assert_equal players @@ Table.get_active_players table_modified;
  let table_temp = (Table.advance_turn (Table.advance_turn (Table.advance_turn table))) in
  assert_equal player_super_evil_bot @@ Table.get_player_at_turn table_temp;
  assert_equal player_tobi @@ Table.get_player_at_turn (Table.advance_turn table_temp)

let test_table_exns _ =
  assert_raises
    (Invalid_argument "Table.init: players list must not be empty")
    (fun () -> Table.init []);
  let table_folded = Table.fold_player (Table.fold_player (Table.fold_player table player_tobi) player_vrinda) player_timmy in
  assert_raises
    (Invalid_argument "Table.advance_turn: no active players")
    (fun () -> Table.advance_turn table_folded)

let series =
  "table_tests" >:::
  [ "player_add_chips" >:: test_player_add_chips
  ; "player_remove_chips" >:: test_player_remove_chips
  ; "player_set_hole_cards" >:: test_player_set_hole_cards
  ; "table_init" >:: test_table_init
  ; "table_advance_turn" >:: test_table_advance_turn
  ; "table_inactive_player" >:: test_table_inactive_player]

