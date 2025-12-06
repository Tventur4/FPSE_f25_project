type difficulty = Easy | Medium | Hard | Expert
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand | MCTS
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type}

let make_move (bot : t) (hole_cards : (Card.t * Card.t) option) (chips : int) : Round.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> Bet of chips
  | Rule_hand_only -> Fold (* unimplemented, decides move based off of its hand only *)
  | Rule_best_hand -> Fold (* unimplemented, decides move based off of the probability it has the best hand *)
  | Rule_MCTS -> Fold (* unimplemented, decides using Monte-Carlo Tree Search algorithm *)
