(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-33"]

open Core

let rec betting_loop (game : Game.t) : Game.t = 
  let round_state = game.current_round in

  if Round.is_over round_state then
    game
  else
    match round_state.to_act with
    | [] -> game
    | player :: _ ->
    let action = 
      match player.player_type with
      | Player.Human ->
        Interface.display_game_state game;
        Interface.display_player_view game;
        Interface.prompt_for_action game
      | Player.Bot bot_data ->
        print_endline (Printf.sprintf "%s is thinking..." player.name);
        
        (*obtain data needed for the bot to make a move*)
        let stage = game.current_round.stage in
        let current_bet = game.current_Round.current_bet in
        let community_cards = game.community_cards in
        let num_players = List.length (Table.get_active_players game.table) in
        let hole = Option.value_exn player.hole_cards in
        let chips = player.chip_stack in
        Bot.make_move bot_data stage current_bet community_cards num_players hole chips
      in

      (*once an action is decided now we need to apply the action*)
      match Round.apply_action round_state player action with
      (*if an error occured, print out the error to let the user now what happened*)
      | Error msg ->
        print_endline (Printf.sprintf "\nERROR: %s\n" msg);
        betting_loop game (* try the same round again *)
      | Ok new_round_state ->
        let new_game = {
          game with current_round = new_round_state;
          pot = new_round_state.pot
        } in
        let action_str = Sexp.to_string (Card.sex_of_action action) in
        print_endline (Printf.sprintf "\n %s perforsm %s\n" player.name action_str);

        betting_loop new_game
      
let rec game_loop (game : Game.t) : unit =
  match Game.current_round game with
  | Card.Showdown ->
    let active_players = Table.get_active_players game.table in

    (*map all the players and find out their best hand rank with Card_set.of_7_cards*)
    let results = List.map active_players ~f:(fun p -> 
      let hole1, hole2 = Option.value_exn p.hole_cards in
      let best_hand = Card_set.of_7_cards (hole_1 :: hole_2 :: game.community_cards) in
      (p, best_hand)
      ) in
    
    let sorted_results = List.sort results ~compare:(fun (_, h1) (_, h2) ->
      Card_set.compare h2 h1
    ) in

    (*display the results to the console*)
    Interface.display_showdown game (List.map sorted_results ~f:(fun (p, h) -> 
      (p, Card_set.to_string h)));
    
    let winner, _ = List.hd_exn sorted_results in
    Interface.announce_winner winner game.pot;

    (* ask if player wants to play again*)
    if Interface.prompt_play_again () then
      let winner_updated = Player.add_chips winner game.pot in
      let new_table = Table.init (Table.current_players game.table) in
      let new_game = Game.init_game new_table in
      game_loop new_game
    else
      print_endline "Thanks for playing Ocaml Hold 'Em"

  | _ ->
    let game_after_betting = betting_loop game in
    let next_game = Game.next_street game_after_betting in
    game_loop next_game


let () =
  let (player_name, num_bots) = Interface.prompt_for_setup () in
  let human = Player.make_player player_name 0 None 1000 in
  let bots = List.init num_bots ~f:(fun i ->
    let bot_data = {
      Bot.diff = Bot.Medium; bot_type = Bot.Rule_best_hand
    } in
    Player.make_player (Printf.sprintf "Bot_%d" (i + 1)) (i + 1) (Some bot_data) 1000
    ) in
  let players = human :: bots in

  try 
    let table = Table.init players in
    let game = Game.init_game table in

    game_loop game
  with 
  | Invalid_argument e -> print_endline (Printf.sprintf "Setup Error: %s" e)
  | Failure e -> print_endline (Printf.sprintf "Runtime Error: %s" e)
