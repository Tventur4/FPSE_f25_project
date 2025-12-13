(*
  The Interface module contains functions to prompt for and handle 
  command-line inputs from human players.
*)

(*functions to print to the console*)

val display_blinds_info : Game.t -> unit
(** [display_blinds_info game] displays the dealer, small blind, big blind, and pot at the start of a hand. *)

val display_game_state : Game.t -> unit
(** [display_game_state game] prints the current public state of the game - community cards, the pot size, 
    chip counts, and the current betting round (flop, turn, etc.) *)

val display_player_view : Game.t -> unit
(** [display_player_view game] prints the private view for the player, including hole cards and available options
    examples: "Call $10", "Raise", "Fold." *)

val display_showdown : Game.t -> (Player.t * string) list -> unit
(** [display_showdown game player_hands] prints the results of the showdown at the end of the rounds.
    Display_showdown should show the hand for each player remaining and announce the winner. 
    The hand should contain the player and then their best 5-card hand. *)

val announce_winner : Player.t -> Chips.t -> unit
(** [announce_winner player amount] prints a simple message stating that
    [player] won a pot of [amount]. used when players bets and everybody else folds. *)

(* functions for reading for the console. *)

val prompt_for_action : Game.t -> Card.action
(** [prompt_for_action game] determines the list of valid actions for the current player 
    (for example you can't [Check] if there's a bet). Console will then prompt 
    the player for input, validates it against the legal moves, and 
    returns the corresponding [action] type. This function will loop until a valid move is entered. *)

val prompt_for_setup : unit -> (string * int * int)
(** [prompt_for_setup ()] asks the user for initial game parameters, such as 
    their name, the number of bot players, and the difficulty of the bots. 
    Initially we're going to test with 1 player and 1 bot.
    returns: [(player_name, num_bots, bot_diff)]. *)

val prompt_play_again : unit -> bool
(** [prompt_play_again ()] asks the user if they wish to play another hand and returns [true] or [false]. *)