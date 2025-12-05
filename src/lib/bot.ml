type difficulty = Easy | Medium | Hard | Expert
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Smart
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type}

let make_move (bot : t) (hole_cards : (Card.t * Card.t) option) (chips : int) : Round.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> Bet of chips
  | Smart -> Fold (* unimplemented *)
