open Core

(* needs to be reworked and refactored.
taking some code from here and dispersing it through
several recursive functions that are going to call each other
and drive the game loop forward
*)
(* let rec betting_loop (game : Game.t) : Game.t = 
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
        let current_bet = game.current_round.current_bet in
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
          game with 
          current_round = new_round_state;
          pot = new_round_state.pot;
          table = new_round_state.table
        } in
        let action_str = Sexp.to_string (Card.sexp_of_action action) in
        print_endline (Printf.sprintf "\n %s performs %s\n" player.name action_str);

        betting_loop new_game
   *)

let rec game_loop (game : Game.t) : unit =
  (*display game state and display the player view*)
  Interface.display_game_state game;
  Interface.display_player_view game;

  if Round.is_over game.current_round then
    match game.current_round.stage with
    (*if the current stage is the showdown then call showdown function
      and determine winner.
      *)
    | Card.Showdown ->
      handle_showdown game
    | _ -> 
      (*otherwise move onto the next street PreFlop -> Flop ...*)
      let next_game = Game.next_street game in
      game_loop next_game
    else
      let current_player = Table.get_player_at_turn game.table in

      let action = 
        match current_player.player_type with
        | Player.Human ->
          Interface.prompt_for_action game
        | Player.Bot bot_data -> 
          print_endline (Printf.sprintf "%s is thinking... " current_player.name);
          (* Unix.sleep 1;  *)
          (* helps for realism, otherwise commands move too fast on the screen*)
          (*obtain data needed for the bot to make a move*)
          let stage = game.current_round.stage in
          let current_bet = game.current_round.current_bet in
          let community_cards = game.community_cards in
          let num_players = List.length (Table.get_active_players game.table) in
          let hole = Option.value_exn current_player.hole_cards in
          let chips = current_player.chip_stack in
          Bot.make_move bot_data stage current_bet community_cards num_players hole chips
      in

    (*let game engine take over and process the turn*)
    match Game.process_turn game action with
    | Error msg ->
      print_endline (Printf.sprintf "\nERROR: %s\n" msg);
      game_loop game (* start game loop over game and see if it works*)
    | Ok game_post_action ->
      let action_str = Sexp.to_string (Card.sexp_of_action action) in
      print_endline (Printf.sprintf "\n%s performs %s\n" current_player.name action_str);
      begin
      (*check if everyone else has folded, and if so determine winner EARLY*)
      match Game.check_winner_by_fold game_post_action with
      | Some winner ->
        handle_winner winner game_post_action.pot game_post_action
      | None ->
        (*other wise keep on going with the game loop*)
        game_loop game_post_action
      end
and handle_showdown (game : Game.t) = 
  let active_players = Table.get_active_players game.table in

  (*find best hands for each of the active players*)
  let results = List.map active_players ~f:(fun p ->
    let hole1, hole2 = Option.value_exn p.hole_cards in
    let all_cards = [hole1; hole2] @ game.community_cards in
    let best_hand = Card_set.of_7_cards all_cards in
    (p, best_hand)
  ) in

  let sorted_results = List.sort results ~compare:(fun (_, h1) (_, h2) ->
    Card_set.compare h2 h1
  ) in

  (*display the reuslts to the console through interface*)
  Interface.display_showdown game (List.map sorted_results ~f:(fun (p, h) ->
    (p, Card_set.to_string h)));

  (*extract the winner by taking the player with the strongest hand
  strength*)
  let winner, _ = List.hd_exn sorted_results in
  handle_winner winner game.pot game

  and handle_winner (winner : Player.t) (pot : Chips.t) (game : Game.t) = 
    Interface.announce_winner winner pot;

    if Interface.prompt_play_again () then
      (* add chips to the winner*)
      let winner_updated = Player.add_chips winner pot in

      (*update the table and replace old winner with new winner*)
      let current_players = Table.current_players game.table in
      
      let next_players = List.map current_players ~f:(fun p ->
        if p.player_id = winner.player_id then winner_updated else p
      ) in

      let new_table = Table.init next_players in
      let new_game = Game.init_game new_table in
      game_loop new_game
      else 
        print_endline "Thanks for Playing OCaml Hold 'Em!"

let () =
  let (player_name, num_bots, bot_diff) = Interface.prompt_for_setup () in
  let human = Player.make_player player_name 0 None (Chips.of_int 1000) in
  let bots = List.init num_bots ~f:(fun i ->
    let bot_data = {
      Bot.diff = Bot.int_to_difficulty bot_diff; bot_type = Bot.Rule_best_hand
    } in
    Player.make_player (Printf.sprintf "Bot_%d" (i + 1)) (i + 1) (Some bot_data) (Chips.of_int 1000)
    ) in
  let players = human :: bots in

  try 
    let table = Table.init players in
    let game = Game.init_game table in

    game_loop game
  with 
  | Invalid_argument e -> print_endline (Printf.sprintf "Setup Error: %s" e)
  | Failure e -> print_endline (Printf.sprintf "Runtime Error: %s" e)
