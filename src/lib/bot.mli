(*
  The Bot module defines an abstract type representing an driving algorithm and behavior of a computer player in a game of poker.
*)

type difficulty = Easy | Medium | Hard | Expert [@@deriving sexp]
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand | MCTS [@@deriving sexp]
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type} [@@deriving sexp]

val make_move : t -> Card.betting_round -> Card.t list -> int -> Card.t * Card.t -> int -> Card.action
(** [make_move bot stage community_cards num_players hole_cards chips] is an action the bot has determined to make given the hole cards and chips of the player. *)

(** abstract out auxiliary functions for complicated bot types to their own .ml files. *)






