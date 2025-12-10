open Core

type difficulty = Easy | Medium | Hard | Expert
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand | MCTS
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type}

let make_move (bot : t) (stage : Card.betting_round) (community_cards : Card.t list) (hole_cards : Card.t * Card.t) (chips : int) : Card.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> Bet of chips
  | Rule_hand_only -> rule_hand_only_move hole_cards chips (* unimplemented, decides move based off of its hand only *)
  | Rule_best_hand -> Fold (* unimplemented, decides move based off of the probability it has the best hand *)
  | Rule_MCTS -> Fold (* unimplemented, decides using Monte-Carlo Tree Search algorithm *)

let rule_hand_only_move (stage : Card.betting_round) (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  match stage with
  | Preflop -> rule_hand_only_move_preflop cards chips
  | Flop -> rule_best_hand_move_flop community_cards cards chips
  | Turn -> rule_best_hand_move_postflop community_cards cards (chips / 2)
  | River -> rule_best_hand_move_postflop community_cards cards chips
  | _ -> Fold

let rule_hand_only_move_preflop (cards : (Card.t * Card.t)) (chips : int) : Round.action =
  let (c1, c2) = cards in
  let r1 = c1.rank in
  let r2 = c2.rank in
  let s1 = c1.suit in
  let s2 = c2.suit in

  let suited = (s1 = s2) in
  let high, low = 
    if compare_rank r1 r2 >= 0 then (r1, r2) else (r2, r1)
  in

  let pair_of rank = (high = rank && low = rank) in

  if pair_of Ace || pair_of King || pair_of Queen || pair_of Jack || (high = Ace && low = King && suited)
  then
    Bet (chips / 5)
  else if pair_of Ten || pair_of Nine || pair_of Eight || (high = Ace && suited && compare_rank low Ten >= 0) || (high = King && suited && low = Queen)
  then
    Call
  else if pair_of Seven || pair_of Six || pair_of Five || pair_of Four || pair_of Three || pair_of Two || (high = King && suited && compare_rank low Ten >= 0) || (high = Queen && suited && low = Jack)
  then
    Check
  else
    Fold

let rule_best_hand_move_flop (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c_2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let hand_value = Card_set.value_of_hand (Card_set.evaluate card_set_hand) in
  if (hand_value > 3) 
  then
    Bet (chips / 10)
  else if (hand_value == 3)
  then
    Call
  else if (hand_value > 0)
  then
    Check
  else Fold

let rec choose_sublists (k : int) (l : 'a list) : 'a list list =
  if k = 0 then [ [] ]
  else 
    let len = List.length l in
    if len < k then []
    else if k = len
      then [ l ]
    else match l with
    | h :: t -> let starting_with_h =
      (List.map (fun sublist -> h :: sublist) (choose_sublists (pred k) t))
      in
      let not_starting_with_h = choose k t in
      starting_with_h @ not_starting_with_h
    | [] -> assert false

let list_max (lst : 'a list) (default_val : 'a) : 'a =
  match lst with
  | [] -> default_val
  | hd :: tl -> List.fold_left max h t

let rule_hand_only_move_postflop (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c_2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let possible_hands = choose_sublists 5 card_set_hand in
  let hand_values = List.map ~f:(function ls -> Card_set.value_of_hand (Card_set.evaluate ls)) possible_hands in
  let hand_value = list_max hand_values 0 in
  if (hand_value > 3)
    then bet (chips / 10)
  else if (hand value == 3)
    then Call
  else if (hand_value > 1)
    then Check
  else Fold

let rule_best_hand_move (game : Game.t) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  Fold

let monte_carlo_tree_search_move (game : Game.t) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  Fold



