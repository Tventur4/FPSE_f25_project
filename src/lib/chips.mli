(*
  The Chips module defines a concrete type for poker chips as well as functions to construct,
  manipulate, and compare chip amounts. This module provides type safety and validation for
  chip operations to prevent invalid states such as negative chip amounts.
*)

type t [@@deriving sexp, compare, equal]
(** [t] is a comparable, serializable type to represent a non-negative amount of poker chips. *)

val zero : t
(** [zero] is the chip amount representing zero chips. *)

val of_int : int -> t
(** [of_int n] creates a chip amount from integer [n]. Raises [Invalid_argument] if [n] is negative. *)

val to_int : t -> int
(** [to_int chips] converts a chip amount to its integer representation. *)

val add : t -> t -> t
(** [add chips1 chips2] returns the sum of [chips1] and [chips2]. *)

val subtract : t -> t -> (t, string) result
(** [subtract chips1 chips2] returns [Ok (chips1 - chips2)] if [chips1 >= chips2],
    otherwise returns [Error "Insufficient chips"]. *)

val ( + ) : t -> t -> t
(** [chips1 + chips2] is the same as [add chips1 chips2]. *)

val ( - ) : t -> t -> (t, string) result
(** [chips1 - chips2] is the same as [subtract chips1 chips2]. *)

val ( >= ) : t -> t -> bool
(** [chips1 >= chips2] returns [true] if [chips1] is greater than or equal to [chips2]. *)

val ( > ) : t -> t -> bool
(** [chips1 > chips2] returns [true] if [chips1] is greater than [chips2]. *)

val ( <= ) : t -> t -> bool
(** [chips1 <= chips2] returns [true] if [chips1] is less than or equal to [chips2]. *)

val ( < ) : t -> t -> bool
(** [chips1 < chips2] returns [true] if [chips1] is less than [chips2]. *)

val ( = ) : t -> t -> bool
(** [chips1 = chips2] returns [true] if [chips1] equals [chips2]. *)

val min : t -> t -> t
(** [min chips1 chips2] returns the smaller of [chips1] and [chips2]. *)

val max : t -> t -> t
(** [max chips1 chips2] returns the larger of [chips1] and [chips2]. *)

val to_string : t -> string
(** [to_string chips] returns the string representation of [chips]. *)

