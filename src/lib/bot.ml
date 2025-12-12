(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-33"]

open Core

type difficulty = Easy | Medium | Hard [@@deriving sexp]
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand [@@deriving sexp]
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type} [@@deriving sexp]

let make_move (bot : t) (stage : Card.betting_round) (current_bet : int) (community_cards : Card.t list) (num_players : int) (hole_cards : Card.t * Card.t) (chips : int) : Card.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> Bet chips
  | Rule_hand_only -> Bot_hand_only.rule_hand_only_move stage current_bet community_cards hole_cards chips (* decides move based off of its hand only *)
  | Rule_best_hand -> Bot_best_hand.rule_best_hand_move stage community_cards num_players hole_cards chips (* decides move based off of the probability it has the best hand *)

