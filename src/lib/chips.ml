open Core

type t = int [@@deriving sexp, compare, equal]
(** [t] is a type representing a non-negative amount of poker chips. *)

let zero : t = 0
(** [zero] is the chip amount representing zero chips. *)

let of_int (n : int) : t =
  if n < 0 then
    invalid_arg (Printf.sprintf "Chips.of_int: negative amount %d" n)
  else
    n

let to_int (chips : t) : int = chips

let add (chips1 : t) (chips2 : t) : t =
  chips1 + chips2

let subtract (chips1 : t) (chips2 : t) : (t, string) result =
  if chips1 >= chips2 then
    Ok (chips1 - chips2)
  else
    Error "Insufficient chips"

let ( + ) = add

let ( - ) = subtract

let ( >= ) (chips1 : t) (chips2 : t) : bool =
  chips1 >= chips2

let ( > ) (chips1 : t) (chips2 : t) : bool =
  chips1 > chips2

let ( <= ) (chips1 : t) (chips2 : t) : bool =
  chips1 <= chips2

let ( < ) (chips1 : t) (chips2 : t) : bool =
  chips1 < chips2

let ( = ) (chips1 : t) (chips2 : t) : bool =
  chips1 = chips2

let min (chips1 : t) (chips2 : t) : t =
  Int.min chips1 chips2

let max (chips1 : t) (chips2 : t) : t =
  Int.max chips1 chips2

let to_string (chips : t) : string =
  Int.to_string chips

