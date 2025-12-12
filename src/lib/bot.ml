(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-33"]

open Core

type difficulty = Easy | Medium | Hard | Expert [@@deriving sexp]
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand | MCTS [@@deriving sexp]
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type} [@@deriving sexp]

let rule_hand_only_move_preflop (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c2) = cards in
  let r1 = c1.rank in
  let r2 = c2.rank in
  let s1 = c1.suit in
  let s2 = c2.suit in

  let suited = (Card.compare_suit s1 s2 = 0) in
  let high, low = 
    if Card.compare_rank r1 r2 >= 0 then (r1, r2) else (r2, r1)
  in

  let pair_of rank = (Card.equal_rank high rank && Card.equal_rank low rank) in

  if pair_of Ace || pair_of King || pair_of Queen || pair_of Jack || (Card.equal_rank high Ace && Card.equal_rank low King && suited)
  then
    Bet (chips / 5)
  else if pair_of Ten || pair_of Nine || pair_of Eight || (Card.equal_rank high Ace && suited && Card.compare_rank low Ten >= 0) || (Card.equal_rank high King && suited && Card.equal_rank low Queen)
  then
    Call
  else if pair_of Seven || pair_of Six || pair_of Five || pair_of Four || pair_of Three || pair_of Two || (Card.equal_rank high King && suited && Card.compare_rank low Ten >= 0) || (Card.equal_rank high Queen && suited && Card.equal_rank low Jack)
  then
    Check
  else
    Fold

let rule_hand_only_move_flop (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let hand_value = Card_set.value_of_hand (Card_set.evaluate card_set_hand) in
  if (hand_value > 3) 
  then
    Bet (chips / 10)
  else if (hand_value = 3)
  then
    Call
  else if (hand_value > 0)
  then
    Check
  else Fold

let list_max (lst : 'a list) (default_val : 'a) : 'a =
  match lst with
  | [] -> default_val
  | hd :: tl -> List.fold ~f:max ~init:hd tl

let rule_hand_only_move_postflop (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let possible_hands = Card_set.choose_sublists 5 card_set_hand in
  let hand_values = List.map ~f:(function ls -> Card_set.value_of_hand (Card_set.evaluate ls)) possible_hands in
  let hand_value = list_max hand_values 0 in
  if (hand_value > 3)
    then Bet (chips / 5)
  else if (hand_value = 3)
    then Call
  else if (hand_value > 1)
    then Check
  else Fold

let shuffle (list : 'a list) : 'a list =
  List.permute list

let rec take (n : int) = function
  | [] -> []
  | hd :: tl -> if n = 0 then [] else hd :: take (n - 1) tl

let rec drop (n : int) = function
  | [] -> []
  | hd :: tl -> if n = 0 then tl else drop (n - 1) tl

let rec deal_opponents n deck acc =
  match n, deck with
  | 0, _ -> (List.rev acc, deck)
  | _, c1 :: c2 :: rest -> deal_opponents (n - 1) rest ((c1, c2) :: acc)
  | _, _ -> (List.rev acc, deck)

let handle_option (o : 'a option) =
  match o with
  | Some v -> v
  | None -> failwith "value not found"

let rec monte_carlo_simulate (k : int) (community_cards : Card.t list) (num_players : int) (hole_cards : (Card.t * Card.t)) (deck : Card.t list) (wins : int) (ties : int) : int * int =
  if k = 0 then (wins, ties)
  else
    let shuffled = shuffle deck in
    let missing = max 0 (5 - List.length community_cards) in
    let new_board = take missing shuffled in
    let rest = drop missing shuffled in
    
    let full_board = community_cards @ new_board in

    let num_opponents = num_players - 1 in

    let opp_holes, rest2 = deal_opponents num_opponents rest [] in

    let (h1, h2) = hole_cards in
    let hero_hand = Card_set.of_7_cards (h1 :: h2 :: full_board) in

    let opp_best =
      match opp_holes with
      | [] -> None
      | _ ->
        let scores =
          List.map
            ~f:(fun (a, b) -> Card_set.of_7_cards (a :: b :: full_board))
            opp_holes
        in
        Some (List.fold ~f:(fun best x -> if Card_set.compare x best > 0 then x else best) ~init:(handle_option (List.hd scores)) (handle_option (List.tl scores)))
    in

    let wins', ties' =
      match opp_best with
      | None -> (wins + 1, ties)
      | Some b ->
        let cmp = Card_set.compare hero_hand b in
        if cmp > 0 then (wins + 1, ties)
        else if cmp = 0 then (wins, ties + 1)
        else (wins, ties)
    in

    monte_carlo_simulate (k - 1) community_cards num_players hole_cards deck wins' ties'

let sorted_deck : Card.t list =
  let suits = [Card.Spades; Card.Hearts; Card.Diamonds; Card.Clubs] in
  let ranks =
    [ Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven
    ; Card.Eight; Card.Nine; Card.Ten; Card.Jack; Card.Queen; Card.King
    ; Card.Ace ]
  in
  List.concat_map suits ~f:(fun suit ->
      List.map ranks ~f:(fun rank -> {Card.suit; rank}))

let estimate_win_probability (community_cards : Card.t list) (num_players : int) (hole_cards : (Card.t * Card.t)) (num_samples : int) : float =
  let (h1, h2) = hole_cards in
  let used = h1 :: h2 :: community_cards in
  let deck = List.filter ~f:(fun c -> not (List.mem used c ~equal:Card.equal)) sorted_deck in
  let wins, ties = monte_carlo_simulate num_samples community_cards num_players hole_cards deck 0 0 in
  (float wins +. 0.5 *. float ties) /. float num_samples

let rule_hand_only_move (stage : Card.betting_round) (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  match stage with
  | PreFlop -> rule_hand_only_move_preflop cards chips
  | Flop -> rule_hand_only_move_flop community_cards cards chips
  | Turn -> rule_hand_only_move_postflop community_cards cards (chips / 2)
  | River -> rule_hand_only_move_postflop community_cards cards chips
  | _ -> Fold

let rule_best_hand_move (stage : Card.betting_round) (community_cards : Card.t list) (num_players : int) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let p = estimate_win_probability community_cards num_players cards 300 in
  match stage with
  | PreFlop ->
    if Float.compare p 0.4 < 0 then Fold
    else if Float.compare p 0.6 < 0 then Call
    else Bet (chips / 10)
  | Flop ->
    if Float.compare p 0.35 < 0 then Fold
    else if  Float.compare p 0.55 < 0 then Call
    else Bet (chips / 10)
  | Turn ->
    if Float.compare p 0.3 < 0 then Fold
    else if Float.compare p 0.55 < 0 then Call
    else Bet (chips / 10)
  | River ->
    if Float.compare p 0.5 < 0 then Fold
    else Bet (chips / 5)
  | _ -> Fold

let make_move (bot : t) (stage : Card.betting_round) (community_cards : Card.t list) (num_players : int) (hole_cards : Card.t * Card.t) (chips : int) : Card.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> Bet chips
  | Rule_hand_only -> rule_hand_only_move stage community_cards hole_cards chips (*cdecides move based off of its hand only *)
  | Rule_best_hand -> rule_best_hand_move stage community_cards num_players hole_cards chips (* unimplemented, decides move based off of the probability it has the best hand *)
  | MCTS -> Fold (* unimplemented, decides using Monte-Carlo Tree Search algorithm *)

