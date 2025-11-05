open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

let unimplemented () =
  failwith "unimplemented"

type suit = Clubs | Diamonds | Hearts | Spades [@@deriving sexp, compare, equal]

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace [@@deriving sexp, compare, equal]

type t = { suit : suit; rank : rank} [@@deriving sexp, compare, equal]

let suit_to_int (s : suit) : int =
  unimplemented ()

let rank_to_int (r : rank) : int =
  unimplemented ()

let card_to_int (card : t) : int =
  unimplemented ()

let int_to_card (index : int) : t =
  unimplemented ()

let to_string (card : t) : string =
  unimplemented ()




