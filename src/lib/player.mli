(*
  The Player module defines a concrete type for players in a game of poker as well as functions to construct, 
  interact with, manipulate, and print them.
*)


type hand = Card.t list [@@deriving sexp]

type t =
  { name : string
  ; player_id : int
  ; folded : bool
  ; is_human : bool
  ; chip_stack : int
  ; hole_cards : (Card.t * Card.t) option
  } [@@deriving sexp]

val make_player : string -> bool -> int -> t
(** [make_player name human chips] is a new player with the specified attributes. *)

val get_name : t -> string
(** [get_name player] is the name of [player]. *)

val get_player_id : t -> int
(** [get_player_id] is the id of [player]. *)

val has_folded  : t -> bool
(** [has_folded player] is true if [player] has folded or cannot make any further bets, and otherwise false. *)

val is_human : t -> bool
(** [is_human player] is true if [player] is a human player, and false if it is a computer player. *)

val get_chip_stack : t -> int
(** [get_chip_stack player] is the chip stack of [player]. *)

val get_hole_cards : t -> (Card.t * Card.t) option
(** [get_hole_cards player] is a tuple containing the two hole cards belonging to [player]. *)

val add_chips : t -> int -> t
(** [add_chips player n] adds [n] chips to the chip stack of [player]. *)

val remove_chips : t -> int -> t
(** [remove_chips player n] removes [n] chips from the chip stack of [player]. *)

val set_hole_cards : t -> (Card.t * Card.t) -> t
(** [set_hole_cards player cards] sets the hole cards of [player] which is now a tuple. *)

(*inside of card_set.mli*)
(* val get_best_hand : t -> C.t list -> hand
(** [get_best_hand player community_cards] is the 5-card hand with the highest possible value, constructed from the 
    two hole cards from [player] and the five [community_cards]. *)

val value_of_hand : hand -> int
(** [value_of_hand hand] is the value of [hand], where a high card has a value of 0, and a royal flush has a 
    value of 9. *) *)