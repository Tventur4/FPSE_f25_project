(*
  The Game module defines a concrete type to represent a game of poker as well as all of the necessary functions to 
  interact with the game.
*)

module C = Card
module D = Deck
module P = Player

type betting_round = PreFlop | Flop | Turn | River | Showdown

type t =
  { hand_number : int 
  ; round : betting_round 
  ; players : P.t list
  ; current_player_index : int
  ; deck : D.t 
  ; community_cards : C.t list
  ; pot : int
  ; big_blind_value : int } [@@deriving sexp]
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

val init_game : P.t list -> int -> t

val get_hand_number : t -> int

val get_round : t -> betting_round

val get_players : t -> P.t list

val get_current_player_index : t -> int

val get_current_player : t -> P.t

val get_deck : t -> D.t

val get_community_cards : t -> C.t list

val get_pot : t -> int

val get_big_blind_value : t -> int

val get_small_blind_value : t -> int

val get_big_bet_value : t -> int





