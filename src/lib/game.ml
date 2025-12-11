open Core

type t =
  { table : Table.t
  ; deck : Deck.t
  ; community_cards : Card.t list
  ; pot : int
  ; current_round : Round.round_state
  }
[@@deriving sexp]

let draw_card deck =
  (* Deck.draw_card : Deck.t -> Card.t * Deck.t *)
  Deck.draw_card deck

let draw_cards deck n =
  (* Deck.draw_cards : Deck.t -> int -> Card.t list * Deck.t *)
  Deck.draw_cards deck n

let burn_one deck =
  (* draw a card and drop it, returning the new deck *)
  let (_card, deck') = Deck.draw_card deck in
  deck'

let replace_player_in_list players updated_player =
  List.map players ~f:(fun p ->
    if Int.equal p.player_id updated_player.player_id then updated_player else p)

let update_player_in_table table updated_player =
  { table with players = replace_player_in_list table.players updated_player }

let get_player_by_index table idx =
  List.nth_exn table.players idx

let num_players table = List.length table.players

let deal_hole_cards_to_table (table : Table.t) (deck : Deck.t) :
  Table.t * Deck.t =
  (* sequentially deal two cards to each player (in table.players order),
     return updated table and remaining deck *)
  let rec loop players deck acc_players =
    match players with
    | [] -> (List.rev acc_players, deck)
    | p :: rest ->
      let (c1, deck1) = draw_card deck in
      let (c2, deck2) = draw_card deck1 in
      let p' = Player.set_hole_cards p (c1, c2) in
      loop rest deck2 (p' :: acc_players)
  in
  let (new_players, deck') = loop table.players deck [] in
  ({ table with players = new_players }, deck')

let next_stage_of (stage : Round.betting_round) : Round.betting_round =
  match stage with
  | Round.PreFlop -> Round.Flop
  | Round.Flop -> Round.Turn
  | Round.Turn -> Round.River
  | Round.River -> Round.Showdown
  | Round.Showdown -> Round.PreFlop

let advance_round_state (state : Round.round_state) (table : Table.t) :
  Round.round_state =
  let new_stage = next_stage_of state.stage in
  {
    state with
    stage = new_stage;
    current_bet = 0;
    (* reset to_act to the currently active players at the table *)
    to_act = Table.get_active_players table;
    (* leave folded and contributions as-is; pot kept as-is *)
  }

let init_game (table : Table.t) : t =
  (* shuffle standard deck, deal hole cards, initialize Round state with players in table order *)
  let deck = Deck.shuffle Deck.sorted_deck in
  let (table_with_hole, deck_after_deal) = deal_hole_cards_to_table table deck in
  let players = Table.current_players table_with_hole in
  let round_state = Round.init players in
  { table = table_with_hole
  ; deck = deck_after_deal
  ; community_cards = []
  ; pot = round_state.pot
  ; current_round = round_state
  }

let current_round (g : t) : Round.betting_round = g.current_round.stage

let advance_round (st : Round.round_state) : Round.round_state =
  (* generic stage advance that does not know about table (keeps to_act as-is) *)
  let new_stage = next_stage_of st.stage in
  { st with stage = new_stage; current_bet = 0 }

let next_street (g : t) : t =
  match g.current_round.stage with
  | Round.PreFlop ->
    (* burn 1, deal 3 -flop *)
    let deck1 = burn_one g.deck in
    let (cards, deck2) = draw_cards deck1 3 in
    let round' = advance_round_state g.current_round g.table in
    let pot' = round'.pot in
    { g with deck = deck2; community_cards = g.community_cards @ cards; current_round = round'; pot = pot' }
  | Round.Flop ->
    (* burn 1, deal 1 (turn) *)
    let deck1 = burn_one g.deck in
    let (cards, deck2) = draw_cards deck1 1 in
    let round' = advance_round_state g.current_round g.table in
    let pot' = round'.pot in
    { g with deck = deck2; community_cards = g.community_cards @ cards; current_round = round'; pot = pot' }
  | Round.Turn ->
    (* burn 1, deal 1 -river *)
    let deck1 = burn_one g.deck in
    let (cards, deck2) = draw_cards deck1 1 in
    let round' = advance_round_state g.current_round g.table in
    let pot' = round'.pot in
    { g with deck = deck2; community_cards = g.community_cards @ cards; current_round = round'; pot = pot' }
  | Round.River ->
    (* move to showdow *)
    let round' = advance_round_state g.current_round g.table in
    { g with current_round = round' }
  | Round.Showdown ->
    
    let winners = Table.get_active_players g.table in
    let n = List.length winners in
    if n = 0 then
      (* no winners, keep pot as is but reset hand *)
      let table' = Table.rotate g.table in
      let deck' = Deck.shuffle g.deck in
      let round' = Round.init (Table.current_players table') in
      { table = table'; deck = deck'; community_cards = []; pot = 0; current_round = round' }
    else
      let share = g.pot / n in
      let remainder = g.pot - (share * n) in
      let rec award players acc_players =
        match players with
        | [] -> List.rev acc_players
        | p :: rest ->
          let p' = Player.add_chips p share in
          award rest (p' :: acc_players)
      in
      let awarded_players = award winners [] in
      (* put remaindder to first winner if any *)
      let awarded_players =
        match awarded_players with
        | [] -> []
        | first :: rest -> (Player.add_chips first remainder) :: rest
      in
      (* update the full table players list with awarded players *)
      let players_after_award =
        List.fold awarded_players ~init:g.table.players ~f:(fun acc_table_players p_aw ->
          List.map acc_table_players ~f:(fun p ->
            if Int.equal p.player_id p_aw.player_id then p_aw else p))
      in
      let table' = { g.table with players = players_after_award } in
      let table_rot = Table.rotate table' in
      let deck' = Deck.shuffle g.deck in
      let round' = Round.init (Table.current_players table_rot) in
      { table = table_rot; deck = deck'; community_cards = []; pot = 0; current_round = round' }


let get_players (g : t) : Player.t list = Table.current_players g.table
let get_current_player_index (g : t) : int = g.table.dealer
let get_current_player (g : t) : Player.t = Table.get_player_at_turn g.table
let get_deck (g : t) : Deck.t = g.deck
let get_community_cards (g : t) : Card.t list = g.community_cards
let get_pot (g : t) : int = g.pot


let small_blind_bet (g : t) (amount : int) : t =
  let n = num_players g.table in
  if n = 0 then invalid_arg "small_blind_bet: no players"
  else
    let sb_idx = (g.table.dealer + 1) mod n in
    let sb = get_player_by_index g.table sb_idx in
    let sb' = Player.remove_chips sb amount in
    let table' = update_player_in_table g.table sb' in
    (* update round contributions and pot *)
    let contributions' = Round.update_contribution g.current_round sb amount in
    let round' = { g.current_round with pot = g.current_round.pot + amount; contributions = contributions' } in
    { g with table = table'; pot = g.pot + amount; current_round = round' }

let big_blind_bet (g : t) (amount : int) : t =
  let n = num_players g.table in
  if n = 0 then invalid_arg "big_blind_bet: no players"
  else
    let bb_idx = (g.table.dealer + 2) mod n in
    let bb = get_player_by_index g.table bb_idx in
    let bb' = Player.remove_chips bb amount in
    let table' = update_player_in_table g.table bb' in
    let contributions' = Round.update_contribution g.current_round bb amount in
    let round' = { g.current_round with pot = g.current_round.pot + amount; contributions = contributions' } in
    { g with table = table'; pot = g.pot + amount; current_round = round' }



let apply_action (g : t) (player : Player.t) (action : Round.action) : (t, string) result =
  let old_contrib = Round.get_contribution g.current_round player in
  match Round.apply_action g.current_round player action with
  | Error e -> Error e
  | Ok new_round_state ->
    let new_contrib = Round.get_contribution new_round_state player in
    let spent = new_contrib - old_contrib in
    let table' =
      if spent = 0 then g.table
      else
        let p_in_table = List.find_exn g.table.players ~f:(fun p -> Int.equal p.player_id player.player_id) in
        let p_after = Player.remove_chips p_in_table spent in
        update_player_in_table g.table p_after
    in
    let g' = { g with current_round = new_round_state; table = table'; pot = new_round_state.pot } in
    Ok g'

(* wrappers for common actions *)
let call g player =
  apply_action g player Round.Call

let raise_ g player amount =
  apply_action g player (Round.Raise amount)

let fold g player =
  apply_action g player Round.Fold

let end_game (_g : t) : unit = ()
