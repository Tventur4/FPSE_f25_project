open Core

type difficulty = Easy | Medium | Hard | Expert
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand | MCTS
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type}

let make_move (bot : t) (game : Game.t) (hole_cards : Card.t * Card.t) (chips : int) : Round.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> Bet of chips
  | Rule_hand_only -> rule_hand_only_move hole_cards (chips / 10) (* unimplemented, decides move based off of its hand only *)
  | Rule_best_hand -> Fold (* unimplemented, decides move based off of the probability it has the best hand *)
  | Rule_MCTS -> Fold (* unimplemented, decides using Monte-Carlo Tree Search algorithm *)

let rule_hand_only_move (cards : (Card.t * Card.t)) (chips : int) : Round.action =
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
    Bet chips
  else if pair_of Ten || pair_of Nine || pair_of Eight || (high = Ace && suited && compare_rank low Ten >= 0) || (high = King && suited && low = Queen)
  then
    Call
  else Fold

