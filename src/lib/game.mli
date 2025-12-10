(*
  The Game module defines a concrete type to represent a game of poker as well as all of the necessary functions to 
  interact with the game.
*)

(* type betting_round = PreFlop | Flop | Turn | River | Showdown [@@deriving sexp] *)
(** [betting_round] is a serializable type to represent the current betting round in a game of Texas Hold 'Em poker. *)

(*UPDATED: t now contains state of a single hand of poker*)
type t =
  { table : Table.t
  ; deck : Deck.t
  ; community_cards : Card.t list
  ; pot : int (*TODO: make a chip module*)
  ; current_round : Round.round_state (*Preflop, flop, showdown.. etc.*)
} [@@deriving sexp]

(** [t] is a record type representing the necessary attributes of a poker game.
    Note that for a given hand, the order of [players] determines the dealer (index 0), small blind (index 1), and 
    big blind (index 2). For the pre-flop round, the player to the left (the subsequent index) of the big blind is 
    the first to bet. For all other betting rounds, the player to the left of the dealer, or the small blind, is the 
    first to bet. At the end of the hand, the order of [players] is rotated via shifting the indices of the players 
    down by 1, i.e. the small blind in the current hand becomes the dealer in the following hand.
    Additionally, the small blind value is half of [big_blind_value]. During the pre-flop and flop rounds, bets and 
    raises must be equal to or greater than [big_blind_value], an amount called the small bet. During the turn and 
    river rounds, bets and raises must be equal to or greater than twice the value of [big_blind_value], an amount 
    called the big bet. *)

(*deals a fresh hand using players at the teble. shuffles deck, deals hole cards, and posts blinds*)
val init_game : Table.t -> t
(** [init_game players big_blind_value] creates and initializes a new game of poker. *)

val current_round : t -> Round.betting_round

(* advanceds to next game stage and deals appropraite cards ot the board*)
val next_street : t -> t

(*advances from one round to another round. in here will be the entire logic of rounds
preflop to flop to ... etc.
a good portion of the entire logic shoudl be in here? hopefully broken down in sub functions idk
*)
val advance_round : Round.round_state -> Round.round_state

(*not needed anymore because we have apply_action in round*)
(* procceses a player's move and updates pot/table
val apply_action : t -> Round.action -> (t, string) result *)

(*Old functions from before game&table split*)

(* val get_hand_number : t -> int
(** [get_hand_number game] gets the current hand number. *)

val get_round : t -> betting_round
(** [get_round game] gets the current betting round. *)

val get_players : t -> P.t list
(** [get_players game] gets a list of the players. *)

val get_current_player_index : t -> int
(** [get_current_player_index game] gets the index of the player whose turn it currently is. *)

val get_current_player : t -> P.t
(** [get_current_player game] gets the current player whose turn it is. *)

val get_deck : t -> D.t
(** [get_deck game] gets the deck associated with [game]. *)

val get_community_cards : t -> C.t list
(** [get_community_cards game] gets the (potentially empty) list of community cards. *)

val get_pot : t -> int
(** [get_pot game] gets the current value of the pot. *)

val get_big_blind_value : t -> int
(** [get_big_blind_value game] gets the amount the big blind has to pay, or the value of the small blind. *)

val get_small_blind_value : t -> int
(** [get_small_blind_value game] returns half of the value of [get_big_blind_value]. *)

val get_call_value : t -> int
(** [get_call_value game] gets the current value of a call. *)

val increment_hand_number : t -> t
(** [increment_hand_number game] increments the hand number once the current hand has concluded. *)

val increment_round : t -> t
(** [increment_round game] increments the betting round to the next round, or [calls increment_hand_number game] if 
    the current betting round is the showdown. *)

val increment_current_player_index : t -> t
(** [increment_current_player_index game] increments the current player index by 1, potentially wrapping around back 
    to 0 if the round is not yet over. *)

val rotate_players : t -> t
(** [rotate players game] rotates the order of players by shifting their indices down by 1 (with the player at index 
    0 wrapping around to the last index). *)

val pre_flop : t -> t
(** [pre_flop game] starts the pre-flop round. Hole cards are dealt to each player, the big/small blind bets are 
    carried out, and then the first round of betting begins. *)

val flop : t -> t
(** [flop game] starts the flop round. Three community cards are dealt, and the second round of betting begins. *)

val turn : t -> t
(** [turn game] starts the turn: a fourth community card is dealt, and the third round of betting begins. *)

val river : t -> t
(** [river game] starts the river: a fifth community card is dealt, and the fourth and final round of betting 
    begins. *)

val showdown : t -> t
(** [showdown game] begins the showdown: all remaining players reveal their hands and the winnings are awarded to the 
    player(s) with the best hand(s). Upon completion, the current hand concludes and the following hand begins. *)

val small_blind_bet : t -> t
(** [small_blind_bet game] makes the small blind player pay the required amount at the start of the pre-flop. *)

val big_blind_bet : t -> t
(** [big_blind_bet game] makes the big blind player pay the required amount at the start of the pre-flop. *)

val call : t -> P.t -> t
(** [call game player] carries out the action of a call for [player]. *)

val raise : t -> P.t -> int -> t
(** [raise game player amount] carries out the action of a raise for [player], where [amount] is the new call value. *)

val fold : t -> P.t -> t
(** [fold game player] carries out the action of folding for [player]. *)

val end_game : t -> unit
* [end_game game] ends the game of poker. *)





