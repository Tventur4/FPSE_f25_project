(*
  The Bot_hand_only module defines auxiliary functions for bots with type rule_best_hand.
  This bot decides a move based solely off of their own cards and the probability that their hand is the best hand 
  relative to the other players.
*)

val get_bracket_best_hand : int -> Card.betting_round -> Card.t list -> int -> (Card.t * Card.t) -> int
(** [get_bracket_best_hand diff_index stage community_cards num_players cards] outputs an integer representing the 
    bracket index for the bot, ranging from 0 to 3. The bracket index is a measure of the bot's confidence on its 
    current position, with 0 being the least confident and 3 being the most confident. *)