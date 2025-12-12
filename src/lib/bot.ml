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

let difficulty_to_int (diff : difficulty) : int =
  match diff with
  | Easy -> 0
  | Medium -> 1
  | Hard -> 2
  | Expert -> 3


let all_in_action (current_bet : int) (chips : int) : Card.action =
  if (current_bet = 0)
    then Bet chips
  else if (current_bet < chips)
    then Raise (chips - current_bet)
  else Call

let determine_move_rule_bots (bracket : int) (stage : Card.betting_round) (current_bet : int) (chips : int) : Card.action =
  if chips - current_bet < 0
    then if bracket = 3 then Call else Fold
  else let bet_made = (current_bet <> 0) in
  match bracket, stage, bet_made with
  | 0, _, _ -> Fold
  | 1, _, false -> Check
  | 1, _, true -> Fold 
  | 2, _, false -> Check
  | 2, _, true -> Call
  | 3, PreFlop, false -> Bet (chips / 10)
  | 3, Flop, false -> Bet (chips / 10)
  | 3, Turn, false -> Bet (chips / 5)
  | 3, River, false -> Bet (chips / 2)
  | 3, PreFlop, true -> Call
  | 3, Flop, true -> Call
  | 3, Turn, true -> Raise ((chips - current_bet) / 5)
  | 3, River, true -> Raise ((chips - current_bet) / 2)
  | _, _, _ -> Fold

let make_move (bot : t) (stage : Card.betting_round) (current_bet : int) (community_cards : Card.t list) (num_players : int) (hole_cards : Card.t * Card.t) (chips : int) : Card.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> all_in_action current_bet chips
  | Rule_hand_only -> 
    let bracket = Bot_hand_only.get_bracket_hand_only (difficulty_to_int bot.diff) stage community_cards hole_cards in
    determine_move_rule_bots bracket stage current_bet chips (* decides move based off of its hand only *)
  | Rule_best_hand -> 
    let bracket = Bot_best_hand.get_bracket_best_hand (difficulty_to_int bot.diff) stage community_cards num_players hole_cards in
    determine_move_rule_bots bracket stage current_bet chips (* decides move based off of the probability it has the best hand *)

