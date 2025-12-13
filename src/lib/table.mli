(*
  The Table module defines a concrete type to represent a table of poker as well as all of the 
  necessary functions to interact with the table. A table module manages the overall game 
  environment, including players seated, chips, dealer position, active hand/round, and turn order.
*)

(*updated: circular-ish data structure of list of players. will use first indexed position as current turn player*) 

(*hide type*)
type t [@@deriving sexp]

(* 
type table = {
  players : Player.t list;   (* seated players in order *)
  dealer : int;               (* index into players list *)
} *)

val init : Player.t list -> t
(** [init players] creates a table with the players in seated order. *)

val rotate: t -> t
(** [rotate t] moves the dealer position one spot. *)

val current_players: t -> Player.t list
(** [current_players t] returns the current players at the table. *)

val get_active_players : t -> Player.t list
(** [get_active_players t] returns players who haven't folded/busted. *)

val get_player_at_turn : t -> Player.t
(** [get_player_at_turn t] returns the player whose turn it is. *)

val advance_turn : t -> t
(** [advance_turn t] advances the turn marker to the next active player. *)

val fold_player : t -> Player.t -> t
(** [fold_player t p] marks player [p] as folded in the table. *)

val update_players : t -> Player.t list -> t
(** [update_players t next_players] updates the players in the table. *)

val reset_all_folded : t -> t
(** [reset_all_folded table] resets all players' folded status to false for a new hand. *)

(* val create : Player.t list -> table
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
* Returns all players seated at the table. *)