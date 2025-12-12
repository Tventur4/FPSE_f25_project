(*
  The Round module defines a concrete type to represent a round of poker as well as all of the 
  necessary functions to interact with the round. A round of poker consists of the actions/bets/state 
  updates that occur before moving to the next round within a hand.
*)

type round_state = {
  stage : Card.betting_round;
  pot : Chips.t;
  current_bet : Chips.t;
  table : Table.t;
  to_act : Player.t list;      (* players in order who still need to act *)
  folded : Player.t list;      (* players who have folded this round *)
  contributions : (int * Chips.t) list;  (* amounts contributed this round *)
} [@@deriving sexp]

val init : Table.t -> round_state
(** [init players] creates a fresh betting round with all players active. *)

val get_contribution : round_state -> Player.t -> Chips.t
(** FILL THIS IN *)

val apply_action :
  round_state -> Player.t -> Card.action -> (round_state, string) result
(** Applies a player's action and returns updated round state. *)


(*table.mli takes care of this, not needed anymore??*)
(* val next_player : round_state -> Player.t option
* Returns the next player to act, or None if the round is over. *)

val is_over : round_state -> bool
(** Whether all players have acted and bets are equal or all but one folded. *)

val reset_for_next_stage : round_state -> Card.betting_round -> round_state