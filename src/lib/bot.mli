(*
  The Bot module defines an abstract type representing an driving algorithm and behavior of a computer player in a game of poker.
*)

type difficulty = Easy | Medium | Hard | Expert [@@deriving sexp]
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand [@@deriving sexp]
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type} [@@deriving sexp]

val int_to_difficulty : int -> difficulty
(** [int_to_difficulty diff_index] is the difficulty corresponding to [diff_index], where 0 -> Easy, 1 -> Medium, 
    2 -> Hard, and 3 -> Expert. *)

val make_move : t -> Card.betting_round -> Chips.t -> Card.t list -> int -> Card.t * Card.t -> Chips.t -> Card.action
(** [make_move bot stage current_bet community_cards num_players hole_cards chips] is an action the bot has determined to make given the hole cards and chips of the player. *)

(** abstract out auxiliary functions for complicated bot types to their own .ml files. *)






