open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

type hand = Card.t list [@@deriving sexp]

type player_type = Human | Bot of Bot.t [@@deriving sexp]

type t =
  { name : string
  ; player_id : int
  ; player_type : player_type
  ; folded : bool
  ; chip_stack : int
  ; hole_cards : (Card.t * Card.t) option
  } [@@deriving sexp]

(*TODO: It depends on use case, but maybe for clarity, we would like some duplication and have a make_human and make_bot function, where the make_bot takes a Bot.t, and the make_human does not. The Bot.t option argument just feels a little funny.*)
let make_player (name : string) (id : int) (bot : Bot.t option) (chips : int) : t =
  { name = name
  ; player_id = id
  ; player_type = (match bot with
    | Some b -> Bot b
    | None -> Human)
  ; folded = false
  ; chip_stack = chips
  ; hole_cards = None
  }

let add_chips (player : t) (chips : int) : t =
  { player with chip_stack = player.chip_stack + chips }

let remove_chips (player : t) (chips: int) : t =
  if chips > player.chip_stack then failwith "illegal argument"
  else { player with chip_stack = player.chip_stack - chips }

let set_hole_cards (player : t) (hole_cards : (Card.t * Card.t)) : t =
  { player with hole_cards = Some hole_cards }


