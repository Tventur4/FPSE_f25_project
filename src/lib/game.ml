[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
open Core

(* Blind amounts *)
let small_blind_amount = Chips.of_int 5
let big_blind_amount = Chips.of_int 10

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

(* Post small blind and big blind at the start of a hand *)
let post_blinds (state : Round.round_state) : (Round.round_state, string) result =
  let players = Table.current_players state.table in
  let num_players = List.length players in
  
  if num_players < 2 then
    Error "Need at least 2 players to post blinds"
  else
    (* If 2 players: dealer posts sb, other player posts big blind
       otherwsie: small blind is at index 1, big blind is at index 2 *)
    let small_blind_player, big_blind_player = 
      if num_players = 2 then
        (* Heads-up: dealer (index 0) posts small blind, other player (index 1) posts big blind *)
        (List.nth_exn players 0, List.nth_exn players 1)
      else
        (* Regular: small blind at index 1, big blind at index 2 *)
        (List.nth_exn players 1, List.nth_exn players 2)
    in
    
    (* Post small blind - use min of blind amount and player's stack (all-in protection) *)
    let small_blind_actual = Chips.min small_blind_amount small_blind_player.chip_stack in
    match Player.remove_chips small_blind_player small_blind_actual with
    | Error _ -> Error "Failed to post small blind"
    | Ok updated_sb_player ->
      (* Update tbl w/ new player chip stack *)
      let current_players = Table.current_players state.table in
      let new_players_sb = List.map current_players ~f:(fun p ->
        if p.player_id = small_blind_player.player_id then updated_sb_player else p
      ) in
      let table_after_sb = Table.update_players state.table new_players_sb in
      let to_act_after_sb = List.map state.to_act ~f:(fun p ->
        if p.player_id = small_blind_player.player_id then updated_sb_player else p
      ) in
      let current_sb_contrib = Round.get_contribution state small_blind_player in
      let new_sb_contrib = Chips.add current_sb_contrib small_blind_actual in
      let contributions_after_sb = List.Assoc.add state.contributions small_blind_player.player_id new_sb_contrib ~equal:Int.equal in
      let state_after_sb = {
        state with
          pot = Chips.add state.pot small_blind_actual;
          contributions = contributions_after_sb;
          table = table_after_sb;
          to_act = to_act_after_sb
      } in
      
      (* big blid: use min of blind amount and player's stack *)
      let players_after_sb = Table.current_players state_after_sb.table in
      let updated_bb_player_fresh = 
        List.find_exn players_after_sb ~f:(fun p -> p.player_id = big_blind_player.player_id)
      in
      let big_blind_actual = Chips.min big_blind_amount updated_bb_player_fresh.chip_stack in
      match Player.remove_chips updated_bb_player_fresh big_blind_actual with
      | Error _ -> Error "Failed to post big blind"
      | Ok final_bb_player ->
        (* Update table with new big blind player chip stack *)
        let new_players_bb = List.map players_after_sb ~f:(fun p ->
          if p.player_id = updated_bb_player_fresh.player_id then final_bb_player else p
        ) in
        let table_after_bb = Table.update_players state_after_sb.table new_players_bb in
        let to_act_after_bb = List.map state_after_sb.to_act ~f:(fun p ->
          if p.player_id = updated_bb_player_fresh.player_id then final_bb_player else p
        ) in
        let current_bb_contrib = Round.get_contribution state_after_sb updated_bb_player_fresh in
        let new_bb_contrib = Chips.add current_bb_contrib big_blind_actual in
        let contributions_after_bb = List.Assoc.add state_after_sb.contributions updated_bb_player_fresh.player_id new_bb_contrib ~equal:Int.equal in
        Ok {
          state_after_sb with
            pot = Chips.add state_after_sb.pot big_blind_actual;
            current_bet = big_blind_actual; (*sett current bet to big blind amount *)
            contributions = contributions_after_bb;
            table = table_after_bb;
            to_act = to_act_after_bb
        }

(* this should be the thing driving the game loop,
need to sync table from round to table in game.
*)
let process_turn (game : t) (act : Card.action) : (t, string) result =
  let current_player = Table.get_player_at_turn game.table in
  match Round.apply_action game.current_round current_player act with
  | Error e -> Error (Printf.sprintf "Error processing turn: %s" e)
  | Ok updated_round_state ->
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
  (* Post blinds before starting the betting round *)
  match post_blinds round_state with
  | Error msg -> failwith (Printf.sprintf "Failed to post blinds: %s" msg)
  | Ok round_state_with_blinds ->
    (* Use the updated table from round_state_with_blinds which has players with reduced chip stacks *)
    { table = round_state_with_blinds.table
    ; deck = deck_after_deal
    ; community_cards = []
    ; pot = round_state_with_blinds.pot
    ; current_round = round_state_with_blinds
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
      (* Deal cards first *)
      let players = Table.current_players rotated_table in
      let (deck_after_deal, players_with_cards) =
        List.fold_map players ~init:fresh_deck ~f:(fun current_deck player ->
          let (cards, next_deck) = draw_n_cards current_deck 2 in
          match cards with
          | [c1; c2] ->
            let p_updated = Player.set_hole_cards player (c1, c2) in
            (next_deck, p_updated) 
          | _ -> failwith "Deck Error: Wasn't able to deal enough cards"
        )
      in
      let table_with_cards = Table.init players_with_cards in
      let new_round = Round.init table_with_cards in
      (* Post blinds for the new hand *)
      match post_blinds new_round with
      | Error msg -> failwith (Printf.sprintf "Failed to post blinds: %s" msg)
      | Ok round_state_with_blinds ->
        { table = table_with_cards
        ; deck = deck_after_deal
        ; community_cards = []
        ; pot = round_state_with_blinds.pot
        ; current_round = round_state_with_blinds
        }
