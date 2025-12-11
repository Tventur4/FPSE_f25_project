open Core

type difficulty = Easy | Medium | Hard | Expert
(** [difficulty] determines the relative difficulty of the bot. *)

type bot_type = Always_fold | All_in | Rule_hand_only | Rule_best_hand | MCTS
(** [bot_type] determines how a bot determines what action to perform. *)

type t = 
  { diff : difficulty
  ; bot_type : bot_type}

let make_move (bot : t) (stage : Card.betting_round) (community_cards : Card.t list) (num_players : int) (hole_cards : Card.t * Card.t) (chips : int) : Card.action =
  match bot.bot_type with
  | Always_fold -> Fold
  | All_in -> Bet chips
  | Rule_hand_only -> rule_hand_only_move stage community_cards hole_cards chips (*cdecides move based off of its hand only *)
  | Rule_best_hand -> rule_best_hand_move stage community_cards num_players hole_cards chips (* unimplemented, decides move based off of the probability it has the best hand *)
  | MCTS -> Fold (* unimplemented, decides using Monte-Carlo Tree Search algorithm *)

let rule_hand_only_move_preflop (cards : (Card.t * Card.t)) (chips : int) : Round.action =
  let (c1, c2) = cards in
  let r1 = c1.rank in
  let r2 = c2.rank in
  let s1 = c1.suit in
  let s2 = c2.suit in

  let suited = (s1 = s2) in
  let high, low = 
    if compare_rank r1 r2 >= 0 then (r1, r2) else (r2, r1)
  in

  let pair_of rank = (high = rank && low = rank) in

  if pair_of Ace || pair_of King || pair_of Queen || pair_of Jack || (high = Ace && low = King && suited)
  then
    Bet (chips / 5)
  else if pair_of Ten || pair_of Nine || pair_of Eight || (high = Ace && suited && compare_rank low Ten >= 0) || (high = King && suited && low = Queen)
  then
    Call
  else if pair_of Seven || pair_of Six || pair_of Five || pair_of Four || pair_of Three || pair_of Two || (high = King && suited && compare_rank low Ten >= 0) || (high = Queen && suited && low = Jack)
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
  else if (hand_value == 3)
  then
    Call
  else if (hand_value > 0)
  then
    Check
  else Fold

let list_max (lst : 'a list) (default_val : 'a) : 'a =
  match lst with
  | [] -> default_val
  | hd :: tl -> List.fold_left max hd tl

let rule_hand_only_move_postflop (community_cards : Card.t list) (cards : (Card.t * Card.t)) (chips : int) : Card.action =
  let (c1, c2) = cards in
  let card_set_hand = c2 :: (c1 :: community_cards) in
  let possible_hands = Card_set.choose_sublists 5 card_set_hand in
  let hand_values = List.map (function ls -> Card_set.value_of_hand (Card_set.evaluate ls)) possible_hands in
  let hand_value = list_max hand_values 0 in
  if (hand_value > 3)
    then Bet (chips / 5)
  else if (hand_value == 3)
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
            (fun (a, b) -> Card_set.of_7_cards (a :: b :: full_board))
            opp_holes
        in
        Some (List.fold_left
          (fun best x ->
            if Card_set.compare x best > 0 then x else best)
            (List.hd scores) (List.tl scores))
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

    monte_carlo_simulate (k - 1) board num_players hole_cards deck wins' ties'

let estimate_win_probability (community_cards : Card.t list) (num_players : int) (hole_cards : (Card.t * Card.t)) (num_samples : int) : float =
  let (h1, h2) = hole_cards in
  let used = h1 :: h2 :: community_cards in
  let deck = List.filter (fun c -> not (List.mem c used)) Deck.sorted_deck in
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
    if p < 0.4 then Fold
    else if p < 0.6 then Call
    else Bet (chips / 10)
  | Flop ->
    if p < 0.35 then Fold
    else if p < 0.55 then Call
    else Bet (chips / 10)
  | Turn ->
    if p < 0.3 then Fold
    else if p < 0.55 then Call
    else Bet (chips / 10)
  | River ->
    if p < 0.5 then Fold
    else Bet (chips / 5)
  | _ -> Fold



