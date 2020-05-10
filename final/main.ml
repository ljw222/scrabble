open Scrabble
open Command
open State

(** [play_game start_of_turn_game current_game player] allows 
    [player] to play the game *)
let rec play_game start_of_turn_game current_game player =
  print_endline("--------------------------------");
  let player_type = 
    match player with
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in 
  basic_info start_of_turn_game current_game player player_type;
  try 
    match Command.parse (read_line ()) with
    | Cell c -> 
      cell_helper start_of_turn_game current_game player c;
    | Tile t -> 
      ANSITerminal.(print_string [red]
                      "\nYou must enter a cell location first. Try again\n");
      play_game start_of_turn_game current_game player
    | Check -> 
      check_helper start_of_turn_game current_game player
    | Quit -> print_endline "Thanks for playing!";
    | Stuck -> stuck_helper start_of_turn_game current_game player 
    | _ -> failwith ""
  with _ -> 
    ANSITerminal.(print_string [red]
                    "\nThis is not a valid command. Try again\n");
    play_game start_of_turn_game current_game player

(** [basic_info start_of_turn_game current_game player player_type] prints 
    the basic game information at the start of every move *)
and basic_info start_of_turn_game current_game player player_type = 
  print_endline("Current Player: " ^ player_type);
  let current_score2 = 
    Scrabble.return_current_score2 player_type current_game in
  print_endline("Score as of beginning of turn:" ^ 
                string_of_int(current_score2));
  print_endline("");
  Scrabble.print_board current_game.board;
  Scrabble.print_hand player current_game;
  ANSITerminal.(print_string [blue]
                  "\nEnter 'cell (_,_)' to pick a cell to play\n")

(** [cell_helper start_of_turn_game current_game player c] deals with the game 
      when [player] plays the command cell [c] in the [current_game] *)
and cell_helper start_of_turn_game current_game player c = 
  ANSITerminal.(print_string [blue]
                  "\nEnter 'tile _' or 'remove'\n");
  let first = (Char.code (String.get (List.nth c 0) 1)) - 48 in 
  let second = (Char.code (String.get (List.nth c 0) 3)) - 48 in 
  match Command.parse (read_line ()) with
  | Tile t -> 
    let new_game_state = 
      Scrabble.play (first,second) (String.get (List.nth t 0) 0) 
        current_game player 
    in
    play_game start_of_turn_game new_game_state player
  | Remove -> 
    let new_game_state = 
      Scrabble.delete (first,second) current_game player start_of_turn_game 
        current_game in 
    play_game start_of_turn_game new_game_state player


  | Quit -> print_endline "Thanks for playing!"
  | _ -> 
    ANSITerminal.(print_string [red]
                    "\nPlease enter a tile command. Try again\n");
    play_game start_of_turn_game current_game player

(** [stuck_helper start_of_turn_game current_game player] deals with the game 
    when [player] gets stuck in the game *)
and stuck_helper start_of_turn_game current_game player = 
  ANSITerminal.(print_string [blue] "\nYour turn is skipped\n");
  ANSITerminal.(print_string [green] "Switch Player\n");
  if (player = Scrabble.Player1 (Scrabble.get_init_player1 ()))
  then play_game start_of_turn_game current_game 
      (Scrabble.Player2 (Scrabble.get_init_player2 ()))
  else play_game start_of_turn_game current_game
      (Scrabble.Player1 (Scrabble.get_init_player1 ()))

(** [check_helper start_of_turn_game current_game player] deals with the game 
    when [player] wants their turn to be checked *)
and check_helper start_of_turn_game current_game player =
  let player_type = 
    match player with
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in 
  let current_score2 = Scrabble.return_current_score2 player_type current_game 
  in
  let turn_score = Scrabble.points_of_turn start_of_turn_game current_game in
  let new_score = current_score2 + turn_score in
  if Scrabble.check_if_valid start_of_turn_game current_game then (
    ANSITerminal.(print_string [blue]
                    "\nOther player please confirm words\n");
    Scrabble.print_words start_of_turn_game current_game;
    print_endline("");
    match Command.parse (read_line ()) with
    | Valid -> if new_score >= State.winning_score (get_init_state ()) then
        begin
          print_endline(player_type ^ " won the game! Yay!");
          exit 0
        end
      else if (player_type = "player1")
      then (
        ANSITerminal.(print_string [green]
                        "\nSwitch player!\n");
        play_game current_game (Scrabble.refill_hand current_game player 
                                  new_score) 
          (Scrabble.Player2 (Scrabble.get_init_player2 ())))
      else (
        ANSITerminal.(print_string [green]
                        "\nSwitch player!\n");
        play_game current_game (Scrabble.refill_hand current_game player 
                                  new_score) 
          (Scrabble.Player1 (Scrabble.get_init_player1 ())))
    | Invalid -> 
      ANSITerminal.(print_string [red]
                      "\nThe other player says that your words aren't valid. 
                      Try again\n");
      play_game start_of_turn_game current_game player
    | Quit -> print_endline "Thanks for playing!"
    | _ -> check_helper start_of_turn_game current_game player
  )
  else 
    begin
      ANSITerminal.(print_string [red]
                      "\nPlease enter a valid move. Try again\n");
      play_game start_of_turn_game current_game player
    end

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [blue]
                  "\nWelcome to our Scrabble Inspired Game!\n");
  ANSITerminal.(print_string [blue]
                  "\nHere are some rules with examples:\n\n");
  print_endline("1. Place a tile on the board");
  ANSITerminal.(print_string [green]
                  "cell (1,0)\n");
  ANSITerminal.(print_string [green]
                  "tile A\n");
  print_endline("2. Remove a tile from the board");
  ANSITerminal.(print_string [green]
                  "cell (1,0)\n");
  ANSITerminal.(print_string [green]
                  "remove\n");
  print_endline("3. End turn by having the other player to check your words");
  ANSITerminal.(print_string [green]
                  "check\n");   
  print_endline("4. When checking the words made of another player");
  ANSITerminal.(print_string [green]
                  "valid or invalid\n");
  print_endline("5. If you don't have a move and want to skip your turn");
  ANSITerminal.(print_string [green]
                  "stuck\n");
  print_endline("6. Quit the game anytime with command");
  ANSITerminal.(print_string [green]
                  "quit\n");
  ANSITerminal.(print_string [blue]
                  "\nType 'start' to begin!\n");
  print_string  "> ";
  match read_line () with
  | "start" -> 
    let winning_score = State.winning_score (State.get_init_state ()) in
    print_endline("WINNING SCORE: " ^ (Int.to_string winning_score));
    play_game (Scrabble.get_init_state ()) (Scrabble.get_init_state ()) 
      (State.player_turn (State.get_init_state ()))
  | _ -> main ()

(* Execute the game engine. *)
let () = main ()


