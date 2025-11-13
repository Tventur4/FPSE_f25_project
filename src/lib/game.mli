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
  ; deck : D.t 
  ; community_cards : C.t list
  ; pot : int
  ; big_blind_value : int }