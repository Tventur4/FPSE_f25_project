(*
  The Card_set module defines a concrete type for a player's hand--a set of five cards chosen from a player's two 
  hole cards and five community cards--as well as functions to construct, [?], and print them.
*)

type t [@@deriving sexp]

val evaluate : Card.t list -> t

val choose_sublists : int -> 'a list -> 'a list list

(* returns best possible 5-card hand constructed from 7 available hands. takes two hole cards and
community cards as parameters*)
val of_7_cards : Card.t list -> t

(* return comparable integer score used to rank hands*)
val value_of_hand : t -> int

(*see if one hand beats another and return integer representing clash.
  1 if hand1 beats hand2, -1 if h2 beats h1, and 0 if tie*)
val compare : t -> t -> int

(*prints nicer hand description*)
val to_string : t -> string