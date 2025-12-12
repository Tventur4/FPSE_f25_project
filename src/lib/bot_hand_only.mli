(*
  The Bot_hand_only module defines auxiliary functions for bots with type rule_hand_only.
  This bot decides a move based solely off of their own cards and does not consider the cards of the other players.
*)

val evaluate_hold_cards : Card.t * Card.t -> int
(** [evaluate_hold_cards cards] is a bracket index for the bot determined based on their hole cards [cards]. See 
    [get_bracket_hand_only] for more information on the bracket index. *)

val evaluate_hand : int -> Card.betting_round -> Card.t list -> Card.t * Card.t -> int
(** [evaluate_hand diff_index stage community_cards cards] is a bracket index for the bot determined based on their 
    hole cards [cards] and community cards. See [get_bracket_hand_only] for more information on the bracket index. *)

val get_bracket_hand_only : int -> Card.betting_round -> Card.t list -> Card.t * Card.t -> int
(** [get_bracket_hand_only diff_index stage community_cards cards] outputs an integer representing the bracket index 
    for the bot, ranging from 0 to 3. The bracket index is a measure of the bot's confidence on its current position, 
    with 0 being the least confident and 3 being the most confident. *)