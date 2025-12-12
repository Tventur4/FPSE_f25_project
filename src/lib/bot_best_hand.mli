(*
  The Bot_hand_only module defines auxiliary functions for bots with type rule_best_hand.
  This bot decides a move based solely off of their own cards and the probability that their hand is the best hand 
  relative to the other players.
*)

val monte_carlo_simulate : int -> Card.t list -> int -> Card.t * Card.t -> Card.t list -> int -> int -> int * int
(** [monte_carlo_simulate k community_cards num_players hole_cards deck wins ties] runs [k] monte-carlo simulations in 
    which the unknown cards (hole cards of other players, unrevealed community cards) are randomly simulated.
    While iterating through the simulations, the algorithm counts how many simulations the bot wins and how many
    simulations the bot ties, and returns these counts as an ordered pair. *)

val estimate_win_probability : Card.t list -> int -> Card.t * Card.t -> int -> float
(** [estimate_win_probability community_cards num_players hole_cards num_samples] gets the estimated probability that 
    the bot wins the current hand. It calculates this probability by running [num_samples] monte-carlo simulations, 
    counting the number of wins and ties (ties are counted as a half of a win), and dividing this sum by the total 
    number of samples. *)

val get_bracket_best_hand : int -> Card.betting_round -> Card.t list -> int -> Card.t * Card.t -> int
(** [get_bracket_best_hand diff_index stage community_cards num_players cards] outputs an integer representing the 
    bracket index for the bot, ranging from 0 to 3. The bracket index is a measure of the bot's confidence on its 
    current position, with 0 being the least confident and 3 being the most confident. *)