open Core

type t = {
  players : Player.t list;   
  dealer  : int;             
}
[@@deriving sexp]

(*Helper fuctionns*)
(* Make sure players list is't empty *)
let len_exn lst =
  let n = List.length lst in
  if n = 0 then invalid_arg "Table: players list must not be empty" else n

(* rotate list left by k positions *)
let rotate_left lst k =
  let n = List.length lst in
  if n = 0 then lst
  else
    let k = Int.rem (k % n + n) n in
    if k = 0 then lst
    else
      let prefix, suffix = List.split_n lst k in
      suffix @ prefix

(* player is active if they are not folded and have chips *)
(* TODO: add to player instead? *)
let player_is_active (p : Player.t) : bool =
  let chip_stack_int = Chips.to_int p.chip_stack in
  not p.folded && chip_stack_int > 0

(*End of helper fucntions*)

(* Table implementation *)

(*create ltable with players in seated order*)
let init (players : Player.t list) : t =
  if List.is_empty players then invalid_arg "Table.init: players list must not be empty"
  else { players; dealer = 0 }

(*move dealer position one spot, clockwise*)
let rotate (t : t) : t =
  let n = len_exn t.players in
  { t with dealer = (t.dealer + 1) mod n }

(*return current players at the table*)
let current_players (t : t) : Player.t list = t.players

(*returns players who haven't folded/busted. either this method *)
let get_active_players (t : t) : Player.t list =
  List.filter t.players ~f:player_is_active

(*returns the player whose turn it is*)
let get_player_at_turn (t : t) : Player.t =
  match t.players with
  | [] -> invalid_arg "Table.get_player_at_turn: empty players list"
  | hd :: _ -> hd


(*advance turn marker to the next active player*)
let advance_turn (t : t) : t =
  let n = len_exn t.players in
  (* find next active player relative to current head *)
  let rec find_next idx =
    if idx >= n then None
    else
      let p = List.nth_exn t.players idx in
      if player_is_active p then Some idx else find_next (idx + 1)
  in
  match find_next 1 with
  | Some idx ->
      (* rotate left so that players[idx] is new head *)
      let new_players = rotate_left t.players idx in
      (* adjust dealer index to still refer to the same physical seat *)
      let new_dealer = ((t.dealer - idx) mod n + n) mod n in
      { players = new_players; dealer = new_dealer }
  | None ->
      let head = get_player_at_turn t in
      if player_is_active head then t
      else invalid_arg "Table.advance_turn: no active players"

(*mark player as folded in the table list*)
let fold_player (t: t) (p : Player.t) : t =
  let new_players = List.map t.players ~f:(fun player ->
    if player.player_id = p.player_id then { player with folded = true}
    else player
  ) in
  {t with players = new_players}

let update_players (t : t) (next_players : Player.t list) : t = {t with players = next_players}

(*reset all players' folded status to false for a new hand*)
let reset_all_folded (t : t) : t =
  let new_players = List.map t.players ~f:(fun p -> { p with folded = false }) in
  { t with players = new_players }