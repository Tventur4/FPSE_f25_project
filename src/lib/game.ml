open Core

type t =
  { table : Table.t
  ; deck : Deck.t
  ; community_cards : Card.t list
  ; pot : int
  ; current_round : Round.round_state
  } [@@deriving sexp]

let init_game (table : Table.t) : t =
  let deck = Deck.sorted_deck |> Deck.shuffle in
  let round_state = Round.init table in
  { table
  ; deck
  ; community_cards = []
  ; pot = round_state.pot
  ; current_round = round_state
  }

let current_round (g : t) : Card.betting_round =
  g.current_round.stage

let advance_round (rs : Round.round_state) : Round.round_state =
  let open Card in
  let next_stage = match rs.stage with
    | PreFlop -> Flop
    | Flop -> Turn
    | Turn -> River
    | River -> Showdown
    | Showdown -> PreFlop
  in
  { rs with stage = next_stage }

(* helper: draw n cards, returning (cards, deck') *)
let draw_n_cards deck n =
  Deck.draw_cards deck n

let next_street (game : t) : t =
  let stage = game.current_round.stage in
  match stage with
  | Card.PreFlop ->
      (* burn 1, deal flop (3) *)
      let _, deck_after_burn = Deck.draw_card game.deck in
      let (cards, deck') = draw_n_cards deck_after_burn 3 in
      let new_round = Round.reset_for_next_stage game.current_round Card.Flop in
      { game with
        deck = deck'
      ; community_cards = game.community_cards @ cards
      ; current_round = new_round
      ; pot = new_round.pot
      }
  | Card.Flop ->
      (* burn 1, deal turn (1) *)
      let _, deck_after_burn = Deck.draw_card game.deck in
      let (cards, deck') = draw_n_cards deck_after_burn 1 in
      let new_round = Round.reset_for_next_stage game.current_round Card.Turn in
      { game with
        deck = deck'
      ; community_cards = game.community_cards @ cards
      ; current_round = new_round
      ; pot = new_round.pot
      }
  | Card.Turn ->
      (* burn 1, deal river (1) *)
      let _, deck_after_burn = Deck.draw_card game.deck in
      let (cards, deck') = draw_n_cards deck_after_burn 1 in
      let new_round = Round.reset_for_next_stage game.current_round Card.River in
      { game with
        deck = deck'
      ; community_cards = game.community_cards @ cards
      ; current_round = new_round
      ; pot = new_round.pot
      }
  | Card.River ->
      (* advance to showdown *)
      let new_round = Round.reset_for_next_stage game.current_round Card.Showdown in
      { game with
        current_round = new_round
      ; pot = new_round.pot
      }
  | Card.Showdown ->
      (* rotate dealer, reset community, start new round with fresh shuffled deck *)
      let rotated_table = Table.rotate game.table in
      let fresh_deck = Deck.sorted_deck |> Deck.shuffle in
      let new_round = Round.init rotated_table in
      { table = rotated_table
      ; deck = fresh_deck
      ; community_cards = []
      ; pot = new_round.pot
      ; current_round = new_round
      }
