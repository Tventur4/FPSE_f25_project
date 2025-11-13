(*
  The Bot module defines an abstract type representing an driving algorithm and behavior of a computer player in a game of poker.
*)

module C = Card
module P = Player
module G = Game

module type BOTDATA = sig
  type t =
    { name : string
    ; difficulty : int
    ; confidence : int 
    ; randomizer_seed : int} [@@deriving sexp]
  (** [t] is a serializable type containing data regarding the bot. *)

  val get_name : string
  (** [get_name] is the name of the bot. *)

  val get_difficulty : int
  (** [get_difficulty] is the difficulty value of the bot, i.e. how intelligent it is. *)

  val get_confidence : int
  (** [get_confidence] is the confidence value of the bot, i.e. how likely it is to make riskier plays relative to 
      its intelligence. *)

  val get_randomizer_seed : int
  (** [get_randomizer_seed] is the randomizer_seed value of the bot, i.e. a seed influencing how the bot behaves when 
      faced with multiple possible actions that are equally or nearly equally optimal. *)
end

module type BOT = sig
  include BOTDATA

  val make_move : G.t -> P.t -> G.t
  (** [make_move game player] makes a move for the given computer player based off of the current state of the game 
      and the player's attributes, i.e. their hole cards and chip stack. *)

end

module type MAKE = functor (BotData: BOTDATA) -> BOT with type t = BotData.t