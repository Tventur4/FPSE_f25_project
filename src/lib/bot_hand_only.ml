(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-33"]

open Core

let rule_hand_only_move_preflop (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c2) = cards in
  let r1 = c1.rank in
  let r2 = c2.rank in
  let s1 = c1.suit in
  let s2 = c2.suit in

  let suited = (Card.compare_suit s1 s2 = 0) in
  let high, low = 
    if Card.compare_rank r1 r2 >= 0 then (r1, r2) else (r2, r1)
  in

  let pair_of rank = (Card.equal_rank high rank && Card.equal_rank low rank) in

  if pair_of Ace || pair_of King || pair_of Queen || pair_of Jack || (Card.equal_rank high Ace && Card.equal_rank low King && suited)
  then
    Bet (chips / 5)
  else if pair_of Ten || pair_of Nine || pair_of Eight || (Card.equal_rank high Ace && suited && Card.compare_rank low Ten >= 0) || (Card.equal_rank high King && suited && Card.equal_rank low Queen)
  then
    Call
  else if pair_of Seven || pair_of Six || pair_of Five || pair_of Four || pair_of Three || pair_of Two || (Card.equal_rank high King && suited && Card.compare_rank low Ten >= 0) || (Card.equal_rank high Queen && suited && Card.equal_rank low Jack)
  then
    Check
  else
    Fold

let rule_hand_only_move_flop (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let hand_value = Card_set.value_of_hand (Card_set.evaluate card_set_hand) in
  if (hand_value > 3) 
  then
    Bet (chips / 10)
  else if (hand_value = 3)
  then
    Call
  else if (hand_value > 0)
  then
    Check
  else Fold

let list_max (lst : 'a list) (default_val : 'a) : 'a =
  match lst with
  | [] -> default_val
  | hd :: tl -> List.fold ~f:max ~init:hd tl

let rule_hand_only_move_postflop (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let possible_hands = Card_set.choose_sublists 5 card_set_hand in
  let hand_values = List.map ~f:(function ls -> Card_set.value_of_hand (Card_set.evaluate ls)) possible_hands in
  let hand_value = list_max hand_values 0 in
  if (hand_value > 3)
    then Bet (chips / 5)
  else if (hand_value = 3)
    then Call
  else if (hand_value > 1)
    then Check
  else Fold

let rule_hand_only_move (stage : Card.betting_round) (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  match stage with
  | PreFlop -> rule_hand_only_move_preflop cards chips
  | Flop -> rule_hand_only_move_flop community_cards cards chips
  | Turn -> rule_hand_only_move_postflop community_cards cards (chips / 2)
  | River -> rule_hand_only_move_postflop community_cards cards chips
  | _ -> Fold