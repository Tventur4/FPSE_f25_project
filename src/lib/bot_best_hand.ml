(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-33"]

open Core

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