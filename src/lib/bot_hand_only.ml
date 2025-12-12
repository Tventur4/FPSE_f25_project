(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-33"]

open Core

let list_max (lst : 'a list) (default_val : 'a) : 'a =
  match lst with
  | [] -> default_val
  | hd :: tl -> List.fold ~f:max ~init:hd tl

let evaluate_hole_cards (cards : (Card.t * Card.t)) : int =
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
    then 3
  else if pair_of Ten || pair_of Nine || pair_of Eight || (Card.equal_rank high Ace && suited && Card.compare_rank low Ten >= 0) || (Card.equal_rank high King && suited && Card.equal_rank low Queen)
    then 2
  else if pair_of Seven || pair_of Six || pair_of Five || pair_of Four || pair_of Three || pair_of Two || (Card.equal_rank high King && suited && Card.compare_rank low Ten >= 0) || (Card.equal_rank high Queen && suited && Card.equal_rank low Jack)
    then 1
  else
    0

let evaluate_hand (stage : Card.betting_round) (community_cards : Card.t list) (cards : (Card.t * Card.t)) : int =
  let (c1, c2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let possible_hands = Card_set.choose_sublists 5 card_set_hand in
  let hand_values = List.map ~f:(function ls -> Card_set.value_of_hand (Card_set.evaluate ls)) possible_hands in
  let hand_value = list_max hand_values 0 in
  if (hand_value > 3)
    then 3
  else if (hand_value = 3)
    then 2
  else if ((Card.equal_betting_round stage Flop && hand_value > 1) || hand_value > 1)
    then 1
  else 0

let get_value_stage (stage : Card.betting_round) (community_cards : Card.t list) (cards : (Card.t * Card.t)) : int =
  match stage with
  | PreFlop -> evaluate_hole_cards cards
  | x -> evaluate_hand x community_cards cards

let rule_hand_only_move (stage : Card.betting_round) (current_bet : int) (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let value = get_value_stage stage community_cards cards in
  if (chips - current_bet < 0) 
    then if (value = 3) then Call else Fold
  else let bet_made = (current_bet <> 0) in
  match value, stage, bet_made with
  | 0, _, _ -> Fold
  | 1, _, false -> Check
  | 1, _, true -> Fold 
  | 2, _, false -> Check
  | 2, _, true -> Call
  | 3, PreFlop, false -> Bet (chips / 10)
  | 3, Flop, false -> Bet (chips / 10)
  | 3, Turn, false -> Bet (chips / 5)
  | 3, River, false -> Bet (chips / 2)
  | 3, PreFlop, true -> Call
  | 3, Flop, true -> Call
  | 3, Turn, true -> Raise ((chips - current_bet) / 5)
  | 3, River, true -> Raise ((chips - current_bet) / 2)
  | _, _, _ -> Fold