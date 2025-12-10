open Core

(* Helper function to clear the terminal*)
let clear_screen () =
  print_string "\027[2J";
  print_string "\027[H";
  Out_channel.flush stdout

(*Display functions*)

let display_game_state (game : Game.t) : Unit = 
  clear_screen ();

  (* print headers for each new stage declaration*)
  let stage_name = 
    Round.sexp_of_betting_round game.current_round.stage
    |> Sexp.to_string
    |> String.uppercase

  in
  print_endline "--- %s ---\n" stage_name;
  
  (*If the current round isn't in the preflop,
  iterate through the list and display all the community cards*)
  match game.current_round.stage with
  | Round.PreFlop -> ()
  | _ -> 
    printf "Pot: $%d\n" game.pot;
    print_string "Community Cards: [";
    (*small function to output to_string of each community card encountered*)
    List.iter game.community_cards ~f:(fun c -> printf "%s " (Card.to_string c));
    print_endline "]\n"
  
let display_player_view (game: Game.t) : unit =
  (*find human player in table of players to display*)
  let human = List.find game.table.players ~f:(fun p ->
    match p.player_type with | Player.Human -> true | _ -> false)
  in
  match human with
  | None -> print_endline "[ Empty View ]"
  | Some h ->
    printf "[Your View: %s]\n" h.name;

    (*Show human's hole cards*)
    (match h.hole_cards with 
    | Some (c1, c2) ->
        printf "Your Hand: [%s, %s]\n" (Card.to_string c1) (Card.to_string c2)
    | None -> print_endline "Your Hand: Empty (Folded)"    
    );

    (*Display board*)
    if not (List.is_empty game.community_cards) then
      (
        print_string "Board: [";
        List.iter game.community_cards ~f:(fun c -> printf "%s " (Card.to_string c));
        print_endline;
      )
    
    printf "Your Chips: $%d\n" h.chip_stack;

    let call_cost = game.current_round.current_bet - (Round.get_contribution game.current_round h) in
    (*money you owe to stay in the hand*)
    if call_cost > 0 then
      printf "Current Bet to Call: $%d\n" call_cost;

    printf "Pot: $%d\n\n" game.pot

let display_showdown (game : Game.t) (results : (Player.t * string) list) : unit =
  print_endline "--- SHOWDOWN ---";
  printf "Pot: $%d\n" game.pot;

  (*run through each of the players and the highest hand possible they can make*)
  List.iter results ~f:(fun (p, hand_desc) -> 
    let cards_str =
      match p.hole_cards with
      | Some (c1, c2) -> sprintf "[%s, %s]" (Card.to_string c1) (Card.to_string c2)
      | None -> "[NA]"
    in
    printf "%s shows: %s (Hand: %s)\n" p.name cards_str hand_desc
    );
    print_newline ()

(*given a winner and an amount (calculated from the pot) output the winner and the amount they've won*)
let announce_winner (winner : Player.t) (amount : int) : unit =
  printf "%s wins $%d.\n" winner.name amount;
  print_newline ();
  print_endline "[Chip Counts]";
  printf "%s: $%d\n" winner.name (winner.chip_stack + amount);
  print_newline()

(*end of Display functions*)

(*start of Input Functions*)

let prompt_for_setup () : (string * int) =
  clear_screen ();
  print_endline "Welcome to OCAML Hold 'Em!\n";
  print_string "Enter your name:\n";
  Out_channel.flush stdout;
  (*acquire name from inchannel, strip the string that's inputted*)
  let name = match In_channel.input_line In_channel.stdin with
    | Some s -> String.strip s
    | _ -> "Player"
   in
  printf "Hello, %s. How many bots do you want to play against? (1-5):\n> " name;
  Out_channel.flush stdout;
  (*acquire number of bots from inchannel as well*)
  let num_bots =
    match In_channel.input_line In_channel.stdin with
    | Some s -> Option.value (int_of_string_opt (String.strip s)) ~default:1
    | None -> 1
  in
  print_endline "Setting up table...";
  print_endline "Blinds are $5 (Small) / $10 (Big).";
  print_endline "All players start with $1000.\n";
  (*return a tuple of name and num_bots*)
  (name, num_bots)


let prompt_play_again () : bool =
  print_string "Play another hand? ([y]es, [n]o)\n> ";
  Out_channel.flush stdout;
  match In_channel.input_line In_channel.stdin with
  | Some s -> 
    let lower = String.lowercase (String.strip s) in
    String.equal lower "y" || String.equal lower "yes"
  | None -> false

let rec prompt_for_action (game : Game.t) : Round.action = 
  let current_bet = game.current_round.current_bet in

  if current_bet = 0 then
    print_endline "Your action? (Options: [f]old, [c]heck, [b]et [amount])"
  else
    print_endline "Your action? (Options: [f]old, [c]all, [r]aise [amount])";

  print_string "> ";
  Out_channel.flush stdout;

  match In_channel.input_line In_channel.stdin with
  (*keep on recursing, if no input is given*)
  | None -> prompt_for_action game
  | Some input ->
    let parts = String.split 