open Core

type suit = Spades | Hearts | Diamonds | Clubs [@@deriving sexp]

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace [@@deriving sexp]

type t [@@deriving sexp]

val rank_value : t -> int





