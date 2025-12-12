open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

type round_state = {
  stage : Card.betting_round;
  pot : Chips.t;
  current_bet : Chips.t;
  table : Table.t;
  to_act : Player.t list;
  folded : Player.t list;
  contributions : (int * Chips.t) list
} [@@deriving sexp]

(* function to find how much a player has already contributed this round*)
(*TODO: Are you building? List.ASsoc is a typo. Also, why an association list instead of a map? I know that at a certain size, a list is faster than a tree map, but it's more idiomatic to use a map when performance is not a concern (and I don't think it's a concern here).*)
let get_contribution (state : round_state) (player : Player.t) : Chips.t =
  match List.Assoc.find state.contributions player.player_id ~equal:Int.equal with
  | Some amount -> amount
  | None -> Chips.zero

(*update a player's contribution in the list*)
let update_contribution (state : round_state) (player : Player.t) (added_amount : Chips.t) =
  let current = get_contribution state player in
  let new_total = Chips.add current added_amount in
  List.Assoc.add state.contributions player.player_id new_total ~equal:Int.equal

let remove_from_to_act (players : Player.t list) (player : Player.t) : Player.t list = 
  List.filter players ~f:(fun p -> p.player_id <> player.player_id)

let get_all_active_players (state : round_state) (current_actor : Player.t) : Player.t list = 
  state.to_act

(*initial round to Preflop, 0's for current bet and pot and an empty list for folded and contributions. 
The players to act is the initial players list. 
*)
let init (table : Table.t) : round_state =
  {
    stage = PreFlop;
    pot = Chips.zero;
    current_bet = Chips.zero;
    table = table;
    to_act = Table.current_players table;
    folded = [];
    contributions = [];
}

(* round is over if there's only one player left or the to_act list is empty*)
let is_over (state: round_state) : bool = 
  (* let active_count = (List.length state.contributions) + (List.length state.to_act) - (List.length state.folded) in *)
  match state.to_act with
  | [] -> true
  | _ -> false

let apply_action (state: round_state) (player : Player.t) (act : Card.action) :
(round_state, string) result = 
  match state.to_act with
  | [] -> Error "No Players left to act. Round is over"
  (*sanity check to make sure a player isn't acting out of turn*)
  | next :: _ when next.player_id <> player.player_id -> 
    Error (Printf.sprintf "It is not %s's turn" player.name)
  | _ -> 
    (*if the player is valid, calculate costs of the action (fold, check, call, bet, raise)*)
    let current_contrib = get_contribution state player in
    match act with
    | Fold ->
      let new_table = Table.fold_player state.table player in 
      Ok {
        state with
          folded = player :: state.folded; 
          to_act = remove_from_to_act state.to_act player;
          table = Table.advance_turn new_table
      }
    | Check -> 
      if Chips.(current_contrib < state.current_bet) then
        Error "Cannot Check: You have to call the current bet"
      else 
        Ok { 
          state with 
            to_act = remove_from_to_act state.to_act player; 
            table = Table.advance_turn state.table}
    | Call ->
      (match Chips.subtract state.current_bet current_contrib with
      | Error _ -> Error "Invalid bet state"
      | Ok amount_needed ->
        (*subject to change for now but if they don't have enough chips to call just assume ALL-IN*)
        if Chips.(amount_needed > player.chip_stack) then
          Ok {
            state with
              pot = Chips.add state.pot player.chip_stack;
              contributions = update_contribution state player player.chip_stack;
              to_act = remove_from_to_act state.to_act player;
              table = Table.advance_turn state.table
          }
        else
          Ok {
            state with
              pot = Chips.add state.pot amount_needed;
              contributions = update_contribution state player amount_needed;
              to_act = remove_from_to_act state.to_act player;
              table = Table.advance_turn state.table
          })
    | Bet amount ->
      if Chips.(state.current_bet > Chips.zero) then
        Error "Cannot Bet: There is already a wager. You must Raise."
      else if Chips.(amount > player.chip_stack) then
        Error "Not enough chips to bet that amount"
      else
        (*might need to revisit here, need to make sure every other active player has a chance
        to call the new current-bet.*)
        Ok {
          state with
          current_bet = amount;
          pot = Chips.add state.pot amount;
          contributions = update_contribution state player amount;
          table = Table.advance_turn state.table;
          to_act = remove_from_to_act (Table.get_active_players state.table) player
        }
    | Raise amount -> 
      (* let total_wager = current_contrib + amount in *)
      let new_high_bet = Chips.add state.current_bet amount in
      (match Chips.subtract new_high_bet current_contrib with
      | Error _ -> Error "Invalid raise calculation"
      | Ok cost ->
        if Chips.(cost > player.chip_stack) then
          Error "Not enough chips to raise"
        else
          (*Game would have to work on refilling Round.to_act with all the active_players minus this player
          to ensure everybody besides this player acts again*)
          Ok {
            state with 
            current_bet = new_high_bet;
            pot = Chips.add state.pot cost;
            contributions = update_contribution state player cost;
            table = Table.advance_turn state.table;
            to_act = remove_from_to_act (Table.get_active_players state.table) player
          })
      
let reset_for_next_stage (state : round_state) (new_stage : Card.betting_round) : round_state =
  {
    state with
    stage = new_stage;
    current_bet = Chips.zero; (* reset betting requirements *)
    contributions = []; (*clear player contributions for the new street*)
    to_act = Table.get_active_players state.table;
  }