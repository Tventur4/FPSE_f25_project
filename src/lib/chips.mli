type t

val of_int : int -> t
(** Create chips; must be >= 0. *)

val to_int : t -> int
(** Extract integer value. *)

val add : t -> int -> t
(** Add non-negative amount. *)

val remove : t -> int -> (t, string) result
(** Subtract amount; returns Error if insufficient. *)

val can_afford : t -> int -> bool
(** True when amount <= current chips. *)
