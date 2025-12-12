(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-33"]

open Core

type difficulty = Easy | Medium | Hard | Expert [@@deriving sexp]
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand [@@deriving sexp]
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type} [@@deriving sexp]

let int_to_difficulty (diff_index : int) : difficulty =
  match diff_index with
  | 0 -> Easy
  | 1 -> Medium
  | 2 -> Hard
  | 3 -> Expert
  | _ -> Easy

let difficulty_to_int (diff : difficulty) : int =
  match diff with
  | Easy -> 0
  | Medium -> 1
  | Hard -> 2
  | Expert -> 3

let all_in_action (current_bet : Chips.t) (chips : Chips.t) : Card.action =
  if Chips.(current_bet = Chips.zero)
    then Bet chips
  else if Chips.(current_bet < chips)
    then (match Chips.subtract chips current_bet with
         | Ok raise_amount -> Raise raise_amount
         | Error _ -> Call)
  else Call

let get_call_thresholds (diff_index : int) : int * int =
  match diff_index with
  | 0 -> 4, 3
  | 1 -> 4, 2
  | 2 -> 3, 2
  | 3 -> 2, 1
  | _ -> 1, 1

let get_bet_amounts (diff_index : int) : int * int * int * int =
  match diff_index with
  | 0 -> 10, 10, 5, 2
  | 1 -> 10, 9, 4, 2
  | 2 -> 8, 8, 4, 1
  | 3 -> 8, 5, 3, 1
  | _ -> 1, 1, 1, 1

let determine_move_rule_bots (diff_index : int) (bracket : int) (stage : Card.betting_round) (current_bet : Chips.t) (chips : Chips.t) : Card.action =
  (match Chips.subtract chips current_bet with
  | Error _ -> 
    if bracket = 3 then Call else Fold
  | Ok remaining ->
    let current_bet_int = Chips.to_int current_bet in
    let bet_made = not (Chips.(current_bet = Chips.zero)) in
    let call_t1, call_t2 = get_call_thresholds diff_index in
    let bet_a1, bet_a2, bet_a3, bet_a4 = get_bet_amounts diff_index in
    let make_bet = Random.bool () in
    let chips_int = Chips.to_int chips in
    match bracket, stage, bet_made with
    | 0, _, _ -> Fold
    | 1, _, false -> Check
    | 1, _, true -> Fold 
    | 2, _, false -> if make_bet then Bet (Chips.of_int (chips_int / bet_a1)) else Check
    | 2, _, true -> if current_bet_int < (chips_int / call_t1) then Call else Fold
    | 3, PreFlop, false -> Bet (Chips.of_int (chips_int / bet_a1))
    | 3, Flop, false -> Bet (Chips.of_int (chips_int / bet_a2))
    | 3, Turn, false -> Bet (Chips.of_int (chips_int / bet_a3))
    | 3, River, false -> Bet (Chips.of_int (chips_int / bet_a4))
    | 3, PreFlop, true -> if current_bet_int < (chips_int / call_t2) then Call else Fold
    | 3, Flop, true -> if current_bet_int < (chips_int / call_t2) then Call else Fold
    | 3, Turn, true -> 
      let remaining_int = Chips.to_int remaining in
      Raise (Chips.of_int (remaining_int / bet_a3))
    | 3, River, true -> 
      let remaining_int = Chips.to_int remaining in
      Raise (Chips.of_int (remaining_int / bet_a4))
    | _, _, _ -> Fold)

let make_move (bot : t) (stage : Card.betting_round) (current_bet : Chips.t) (community_cards : Card.t list) (num_players : int) (hole_cards : Card.t * Card.t) (chips : Chips.t) : Card.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> all_in_action current_bet chips
  | Rule_hand_only -> 
    let diff_index = difficulty_to_int bot.diff in
    let bracket = Bot_hand_only.get_bracket_hand_only diff_index stage community_cards hole_cards in
    determine_move_rule_bots diff_index bracket stage current_bet chips (* decides move based off of its hand only *)
  | Rule_best_hand -> 
    let diff_index = difficulty_to_int bot.diff in
    let bracket = Bot_best_hand.get_bracket_best_hand diff_index stage community_cards num_players hole_cards in
    determine_move_rule_bots diff_index bracket stage current_bet chips (* decides move based off of the probability it has the best hand *)

