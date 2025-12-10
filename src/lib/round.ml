open Core

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

type action = Fold | Check | Call | Bet of int | Raise of int
[@@deriving sexp]

type betting_round = PreFlop | Flop | Turn | River | Showdown [@@deriving sexp]

type round_state = {
  stage : betting_round;
  pot : int;
  current_bet : int;
  to_act : Player.t list;
  folded : Player.t list;
  contributions : (int * int) list
} [@@deriving sexp]

(* function to find how much a player has already contributed this round*)
(*TODO: Are you building? List.ASsoc is a typo. Also, why an association list instead of a map? I know that at a certain size, a list is faster than a tree map, but it's more idiomatic to use a map when performance is not a concern (and I don't think it's a concern here).*)
let get_contribution (state : round_state) (player : Player.t) : int =
  match List.Assoc.find state.contributions player.player_id ~equal:Int.equal with
  | Some amount -> amount
  | None -> 0

(*update a player's contribution in the list*)
let update_contribution (state : round_state) (player : Player.t) (added_amount : int) =
  let current = get_contribution state player in
  let new_total = current + added_amount in
  List.Assoc.add state.contributions player.player_id new_total ~equal:Int.equal

let remove_from_to_act (players : Player.t list) (player : Player.t) : Player.t list = 
  List.filter players ~f:(fun p -> p.player_id <> player.player_id)

let get_all_active_players (state : round_state) (current_actor : Player.t) : Player.t list = 
  state.to_act

(*initial round to Preflop, 0's for current bet and pot and an empty list for folded and contributions. 
The players to act is the initial players list. 
*)
let init (players : Player.t list) : round_state =
  {
    stage = PreFlop;
    pot = 0;
    current_bet = 0;
    to_act = players;
    folded = [];
    contributions = [];
}

(* round is over if there's only one player left or the to_act list is empty*)
let is_over (state: round_state) : bool = 
  (* let active_count = (List.length state.contributions) + (List.length state.to_act) - (List.length state.folded) in *)
  match state.to_act with
  | [] -> true
  | _ -> false

let apply_action (state: round_state) (player : Player.t) (act : action) :
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
      Ok {
        state with
          folded = player :: state.folded
          ; to_act = remove_from_to_act state.to_act player
      }
    | Check -> 
      if current_contrib < state.current_bet then
        Error "Cannot Check: You have to call the current bet"
      else 
        Ok { state with to_act = remove_from_to_act state.to_act player}
    | Call ->
      let amount_needed = state.current_bet - current_contrib in
      (*subject to change for now but if they don't have enough chips to call just assume ALL-IN*)
      if amount_needed > player.chip_stack then
        Ok {
          state with
            pot = state.pot + player.chip_stack;
            contributions = update_contribution state player player.chip_stack;
            to_act = remove_from_to_act state.to_act player
        }
      else
        Ok {
          state with
            pot = state.pot + amount_needed;
            contributions = update_contribution state player amount_needed;
            to_act = remove_from_to_act state.to_act player
        }
    | Bet amount ->
      if state.current_bet > 0 then
        Error "Cannot Bet: There is already a wager. You must Raise."
      else if amount > player.chip_stack then
        Error "Not enough chips to bet that amount"
      else
        (*might need to revisit here, need to make sure every other active player has a chance
        to call the new current-bet.*)
        Ok {
          state with
          current_bet = amount;
          pot = state.pot + amount;
          contributions = update_contribution state player amount;
          to_act = remove_from_to_act state.to_act player
        }
    | Raise amount -> 
      (* let total_wager = current_contrib + amount in *)
      let new_high_bet = state.current_bet + amount in
      let cost = new_high_bet - current_contrib in

      if cost > player.chip_stack then
        Error "Not enough chips to raise"
      else
        (*Game would have to work on refilling Round.to_act with all the active_players minus this player
        to ensure everybody besides this player acts again*)
        Ok {
          state with 
          current_bet = new_high_bet;
          pot = state.pot + cost;
          contributions = update_contribution state player cost;
          to_act = remove_from_to_act state.to_act player
        }
      