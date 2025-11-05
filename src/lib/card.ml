type suit = Clubs | Diamonds | Hearts | Spades [@@deriving sexp, compare]

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace [@@deriving sexp, compare]

type t = {suit : suit; rank : rank} [@@deriving sexp, compare]

