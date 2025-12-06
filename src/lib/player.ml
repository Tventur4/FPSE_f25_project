open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

type hand = Card.t list [@@deriving sexp]

type player_type = Human | Bot of Bot.t

type t =
  { name : string
  ; player_id : int
  ; player_type : player_type
  ; folded : bool
  ; chip_stack : int
  ; hole_cards : (Card.t * Card.t) option
  } [@@deriving sexp]

let make_player (name : string) (id : int) (bot : Bot.t option) (chips : int) : player =
  { name = name
  ; player_id = id;
  ; player_type = match bot with
    | Some b -> Bot of b
    | None -> Human
  ; folded = false
  ; chip_stack = chips
  ; hole_cards = None}

let add_chips (player : t) (chips : int) : player =
  player with chip_stack = player.chip_stack + chips

let add_chips (player : t) (chips: int) : player =
  if (chips > player.chip_stack) failwith "illegal argument" else
  player with chip_stack = player.chip_stack - chips

let set_hole_cards (player : t) (hole_cards : (Card.t * Card.t)) : t =
  player with hole_cards = Some hole_cards


