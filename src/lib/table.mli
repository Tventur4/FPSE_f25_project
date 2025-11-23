(*
  The Table module defines a concrete type to represent a table of poker as well as all of the 
  necessary functions to interact with the table. A table module manages the overall game 
  environment, including players seated, chips, dealer position, active hand/round, and turn order.
*)

type table = {
  players : Player.t list;   (* seated players in order *)
  dealer : int;               (* index into players list *)
}

val create : Player.t list -> table
(** Create a table with players in given seating order; dealer starts at index 0. *)

val dealer_id : table -> Player.t
(** Returns the player who is currently the dealer. *)

val rotate_dealer : table -> table
(** Moves the dealer button to the next player. *)

val small_blind : table -> Player.t
(** Player immediately left of the dealer. *)

val big_blind : table -> Player.t
(** Player two seats left of the dealer. *)

val next_player : table -> Player.t -> Player.t
(** Returns the next player clockwise from the given player. *)

val active_players : table -> Player.t list
(** Returns all players seated at the table. *)