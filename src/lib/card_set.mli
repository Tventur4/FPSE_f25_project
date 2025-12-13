(*
  The Card_set module defines a concrete type for a player's hand--a set of five cards chosen from a player's two 
  hole cards and five community cards--as well as functions to construct, [?], and print them.
*)

type t [@@deriving sexp]

val evaluate : Card.t list -> t
(** [evaluate cards] turns a 5-card list into a card_set object. *)

val choose_sublists : int -> 'a list -> 'a list list
(** [choose_sublists k list] is a list containing all possible sublists (or subsets, as order is disregarded) [list] 
    of size [k]. *)

val of_7_cards : Card.t list -> t
(** [of_7_cards seven_cards] returns the best possible 5-card hand constructed from 7 available hands. It takes two 
    hole cards and community cards as parameters. *)

val value_of_hand : t -> int
(** [value_of_hand hand] returns a comparable integer score that is used to rank hands. *)

val compare : t -> t -> int
(** [compare h1 h2] sees if one hand beats another and returns an integer representing clash. 
    It returns 1 if h1 beats h2, -1 if h2 beats h1, and 0 if it is tie. *)

val to_string : t -> string
(** [to_string hand] prints nicer hand description. *)