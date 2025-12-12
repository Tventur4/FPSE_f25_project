(*
  Testing file for Round module.
*)
open Core
open OUnit2

let player_tobi = Player.make_player "Tobi" 0 None 100
let player_vrinda = Player.make_player "Vrinda" 1 None 100
let player_timmy = Player.make_player "Timmy" 2 None 100
let players = [player_tobi ; player_vrinda ; player_timmy]

let round = Round.init (Table.init players)

let get_from_result (x : ('a, 'b) result) : 'a =
  match x with
  | Ok x -> x
  | Error s -> invalid_arg s

let test_apply_action_check _ =
  let round1 = get_from_result (Round.apply_action round player_tobi Check) in
  let round2 = get_from_result (Round.apply_action round1 player_vrinda Check) in
  let round3 = get_from_result (Round.apply_action round2 player_timmy Check) in
  assert_equal true @@ Round.is_over round3

let test_apply_action_bet_calls _ =
  let round1 = get_from_result (Round.apply_action round player_tobi (Bet 10)) in
  assert_equal 10 @@ Round.get_contribution round1 player_tobi;
  let round2 = get_from_result (Round.apply_action round1 player_vrinda Call) in
  assert_equal 10 @@ Round.get_contribution round2 player_vrinda;
  let round3 = get_from_result (Round.apply_action round2 player_timmy Call) in
  assert_equal 10 @@ Round.get_contribution round3 player_timmy;
  assert_equal true @@ Round.is_over round3

let test_apply_action_raise _ =
  let round1 = get_from_result (Round.apply_action round player_tobi (Bet 10)) in
  let round2 = get_from_result (Round.apply_action round1 player_vrinda (Raise 15)) in
  assert_equal 25 @@ Round.get_contribution round2 player_vrinda;
  let round3 = get_from_result (Round.apply_action round2 player_timmy Call) in
  assert_equal 25 @@ Round.get_contribution round3 player_timmy;
  assert_equal false @@ Round.is_over round3;
  let round4 = get_from_result (Round.apply_action round3 player_tobi Call) in
  assert_equal 25 @@ Round.get_contribution round4 player_tobi

let series =
  "round_tests" >:::
  [ "apply_action_check" >:: test_apply_action_check
  ; "apply_action_bet_calls" >:: test_apply_action_bet_calls
  ; "apply_action_raise" >:: test_apply_action_raise]
