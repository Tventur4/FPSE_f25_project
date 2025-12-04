(*
  The Round module defines a concrete type to represent a round of poker as well as all of the 
  necessary functions to interact with the round. A round of poker consists of the actions/bets/state 
  updates that occur before moving to the next round within a hand.
*)

type action =
  | Fold
  | Check
  | Call
  | Bet of int
  | Raise of int
[@@deriving sexp]

type betting_round = PreFlop | Flop | Turn | River | Showdown [@@deriving sexp]

type round_state = {
  stage : betting_round;
  pot : int;
  current_bet : int;
  to_act : Player.t list;      (* players in order who still need to act *)
  folded : Player.t list;      (* players who have folded this round *)
  contributions : (Player.t * int) list;  (* amounts contributed this round *)
}

val init : Player.t list -> round_state
(** [init players] creates a fresh betting round with all players active. *)

val apply_action :
  round_state -> Player.t -> action -> (round_state, string) result
(** Applies a player's action and returns updated round state. *)


(*table.mli takes care of this, not needed anymore??*)
(* val next_player : round_state -> Player.t option
* Returns the next player to act, or None if the round is over. *)

val is_over : round_state -> bool
(** Whether all players have acted and bets are equal or all but one folded. *)
