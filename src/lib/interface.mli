(*
  The Interface module contains functions to prompt for and handle 
  command-line inputs from human players.
*)

module C = Card
module P = Player
module G = Game

(** 
[action] is a variant type that represents a player's move. Will be split into [fold], [check], [call],
[bet], and [raise]. these represents the different actions available for player. bet and int both take an int
argument.
*)
type action =
  | Fold
  | Check
  | Call
  | Bet of int
  | Raise of int


(*functions to print to the console*)

(**
 [display_game_state game] prints the current public state of the game - community cards, the pot size, 
 chip counts, and the current betting round (flop, turn, etc.)
*)
val display_game_state : G.t -> unit

(**
 [display_player_view game] prints the private view for the player, including hole cards and available options
 examples: "Call $10", "Raise", "Fold"
*)
val display_player_view : G.t -> unit

(**
 [display_showdown game player_hands] prints the results of the showdown at the end of the rounds.
 Display_showdown should show the hand for each player remaining and announce the winner. 
 The hand should contain the player and then their best 5-card hand.
*)
val display_showdown : G.t -> (P.t * string) list -> unit

(**
 [announce_winner player amount] prints a simple message stating that
 [player] won a pot of [amount]. used when players bets and everybody else folds.
*)
val announce_winner : P.t -> int -> unit

(* functions for reading for the console. *)

(**
 * [prompt_for_action game] determines the list of valid actions for the current player 
  (for example you can't [Check] if there's a bet). Console will then prompt 
  the player for input, validates it against the legal moves, and 
  returns the corresponding [action] type. This function will loop until a valid move is entered.
*)
val prompt_for_action : G.t -> action

(**
 [prompt_for_setup ()] asks the user for initial game parameters, such as 
 their name and the number of bot players. Initially we're going to test with 1 player and 1 bot.
 returns: [(player_name, num_bots)].
*)
val prompt_for_setup : unit -> (string * int)

(**
 [prompt_play_again ()] asks the user if they wish to play another hand and returns [true] or [false].
*)
val prompt_play_again : unit -> bool