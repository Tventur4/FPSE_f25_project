open Core

type t =
  { table : Table.t
  ; deck : Deck.t
  ; community_cards : Card.t list
  ; pot : int (*TODO: make a chip module*)
  ; current_round : Round.round_state (*Preflop, flop, showdown.. etc.*)
} [@@deriving sexp]

(*deals a fresh hand using players at the teble. shuffles deck, deals hole cards, and posts blinds*)
let init_game (table : Table.t) : t =
  let shuffled_deck = Deck.shuffle Deck.sorted_deck in
  let pot = 0 in
  let community_cards = [] in
  let current_round = Round.PreFlop in
  { table; deck = shuffled_deck; community_cards; pot; current_round }

let current_round (game : t) : Round.round_state =
  game.current_round

(*advances from one round to another round. in here will be the entire logic of rounds
preflop to flop to ... etc.
a good portion of the entire logic shoudl be in here? hopefully broken down in sub functions idk
*)
let next_street (game : t) : t =
  match game.current_round with
  | Round.PreFlop ->
    let (flop_cards, new_deck) = Deck.draw_cards game.deck 3 in
    { game with
      community_cards = flop_cards
    ; deck = new_deck
    ; current_round = Round.Flop }
  | Round.Flop ->
    let (turn_card, new_deck) = Deck.draw_card game.deck in
    { game with
      community_cards = game.community_cards @ [turn_card]
    ; deck = new_deck
    ; current_round = Round.Turn }
  | Round.Turn ->
    let (turn_card, new_deck) = Deck.draw_card game.deck in
    { game with
      community_cards = game.community_cards @ [turn_card]
    ; deck = new_deck }
  | Round.River ->
    { game with current_round = Round.Showdown }
  | Round.Showdown ->
    failwith "Game is already in Showdown round"

(*advances from one round to another round. in here will be the entire logic of rounds
preflop to flop to ... etc.*)
let advance_round (game : t) : t =
  if Round.is_over game.current_round then
    next_street game
  else
    failwith "Current round is not over yet"