open Core

type t =
  { table : Table.t
  ; deck : Deck.t
  ; community_cards : Card.t list
  ; pot : Chips.t
  ; current_round : Round.round_state
  } [@@deriving sexp]

(* helper: draw n cards, returning (cards, deck') *)
let draw_n_cards deck n =
  Deck.draw_cards deck n

(* running into bugs where cards are still being dealt
to only one person when everybody else has folded
delegate a winner when everyone else has folded.
*)
let check_winner_by_fold (game : t) : Player.t option =
  match Table.get_active_players game.table with
  | [winner] -> Some winner
  | _ -> None

(* this should be the thing driving the game loop,
need to sync table from round to table in game.
*)
let process_turn (game : t) (act : Card.action) : (t, string) result =
  let current_player = Table.get_player_at_turn game.table in
  match Round.apply_action game.current_round current_player act with
  | Error e -> Error each
  | Okay updated_round_state ->
    let new_table = updated_round_state.table in
    Ok {
      game with
      current_round = updated_round_state;
      table = new_table;
      pot = updated_round_state.pot
    }

let init_game (table : Table.t) : t =
  let deck = Deck.sorted_deck |> Deck.shuffle in
  let players = Table.current_players table in

  let (deck_after_deal, players_with_cards) =
  List.fold_map players ~init:deck ~f:(fun current_deck player ->
    let (cards, next_deck) = draw_n_cards current_deck 2 in
    match cards with
    | [c1; c2] ->
      let p_updated = Player.set_hole_cards player (c1, c2) in
      (next_deck, p_updated) 
    | _ -> failwith "Deck Error: Wasn't able to deal enough cards"
  )
    in
  let table_with_cards = Table.init players_with_cards in
  let round_state = Round.init table_with_cards in
  { table = table_with_cards
  ; deck = deck_after_deal
  ; community_cards = []
  ; pot = round_state.pot
  ; current_round = round_state
  }

let current_round (g : t) : Card.betting_round =
  g.current_round.stage

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
