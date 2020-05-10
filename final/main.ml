open Scrabble
open Command
open State

(** [basic_info start_of_turn_game current_game player] prints the basic 
    information at the start of every move *)
let basic_info start_of_turn_game current_game player = 
  let current_player = State.player_turn player in 
  let player_type = 
    match current_player with
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in 
  print_endline("Current Player: " ^ player_type);
  let player_score = Int.to_string (State.get_player_score player_type player) in
  print_endline("Score as of beginning of turn:" ^ player_score);
  print_endline("");
  Scrabble.print_board current_game.board;
  print_endline("");
  Scrabble.print_hand current_player current_game;
  print_endline("");
  print_string("Enter 'cell (_,_)' to pick a cell to play");
  print_endline("")

let rec play_game start_of_turn_game current_game player =
  print_endline("________________________________");
  let current_player = State.player_turn player in 
  basic_info start_of_turn_game current_game player;
  try 
    match Command.parse (read_line ()) with
    | Cell c -> 
      play_cell_command start_of_turn_game current_game player current_player c
    | Tile t -> 
      print_endline("");
      print_string "You must enter a cell location first. Try again";
      print_endline("");
      play_game start_of_turn_game current_game player
    | Check -> 
      begin
        check_helper start_of_turn_game current_game player
      end
    |_ -> failwith ""
  with _ -> 
    print_endline("");
    print_string "This is not a valid command. Try again";
    print_endline("");
    play_game start_of_turn_game current_game player

and play_cell_command start_of_turn_game current_game player current_player c = 
  begin
    print_string "Enter 'tile _' or 'remove'"; print_endline("");
    match Command.parse (read_line ()) with
    | Tile t -> 
      let first = (Char.code (String.get (List.nth c 0) 1)) - 48 in 
      let second = (Char.code (String.get (List.nth c 0) 3)) - 48 in 
      let new_game_state = 
        Scrabble.play (first,second) (String.get (List.nth t 0) 0) current_game current_player 
      in
      play_game start_of_turn_game new_game_state player
    | Remove -> 
      let first = (Char.code (String.get (List.nth c 0) 1)) - 48 in 
      let second = (Char.code (String.get (List.nth c 0) 3)) - 48 in 
      let new_game_state = Scrabble.delete (first,second) current_game current_player in 
      play_game start_of_turn_game new_game_state player
    | Quit -> 
      begin
        print_endline "Thanks for playing!";
        exit 0
      end
    | _ -> 
      print_endline("");
      print_string "Please enter a tile command. Try again";
      print_endline("");
      play_game start_of_turn_game current_game player
  end

and check_helper start_of_turn_game current_game player =
  let current_player = (State.player_turn player) in
  let current_player_type = match current_player with 
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in
  if Scrabble.check_if_valid start_of_turn_game current_game then (
    print_endline("");
    print_string "Other player please confirm words";
    print_endline("");
    Scrabble.print_words start_of_turn_game current_game;
    print_endline("");
    let new_score = 
      Scrabble.points_of_turn start_of_turn_game current_game in
    match Command.parse (read_line ()) with
    | Valid -> 
      if new_score >= State.winning_score player then
        begin
          print_endline(current_player_type ^ " won the game! Yay!");
          exit 0
        end
      else if (current_player_type = "player1")
      then 
        begin
          print_endline("");
          print_endline("Switch player!");
          let new_player1 = Scrabble.update_player "player1" new_score in 
          let new_state = State.update_player1 player new_player1 in
          play_game current_game 
            (Scrabble.refill_hand current_game new_player1) new_state
        end
      else 
        begin 
          print_endline("");
          print_endline("Switch player!");
          let new_player2 = Scrabble.update_player "player2" new_score in 
          let new_state = State.update_player2 player new_player2 in
          play_game current_game (Scrabble.refill_hand current_game new_player2) 
            new_state
        end
    | Invalid -> print_endline("word not valid");
      play_game start_of_turn_game current_game player
    | Quit -> print_endline "Thanks for playing!"
    | _ -> check_helper start_of_turn_game current_game player
  )
  else (print_endline("");
        print_string "Please enter a valid move. Try again";
        print_endline("");
        play_game start_of_turn_game current_game player)

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [blue]
                  "\nWelcome to our Scrabble Inspired Game!\n");
  ANSITerminal.(print_string [blue]
                  "\nHere are some rules with examples:\n");
  ANSITerminal.(print_string [black]
                  "\n1. Place a tile on the board \n");
  ANSITerminal.(print_string [green]
                  "\ncell (1,0)\n");
  ANSITerminal.(print_string [green]
                  "\ntile A\n");
  ANSITerminal.(print_string [black]
                  "\n2. Remove a tile from the board\n");
  ANSITerminal.(print_string [green]
                  "\ncell (1,0)\n");
  ANSITerminal.(print_string [green]
                  "\nremove\n");
  ANSITerminal.(print_string [black]
                  "\n3. Finish your turn by asking the other player to validate your input\n");
  ANSITerminal.(print_string [green]
                  "\ncheck\n");   
  ANSITerminal.(print_string [black]
                  "\n4. When checking the words made of another player\n");
  ANSITerminal.(print_string [green]
                  "\nvalid or invalid\n");
  ANSITerminal.(print_string [black]
                  "\n5. Quit the game anytime with command\n");
  ANSITerminal.(print_string [green]
                  "\nquit\n");
  ANSITerminal.(print_string [blue]
                  "\nType 'start' to begin!\n");
  print_string  "> ";
  match read_line () with
  | "start" -> 
    begin
      let winning_score = State.winning_score (State.get_init_state ()) in
      print_endline("WINNING SCORE: " ^ (Int.to_string winning_score));
      play_game (Scrabble.get_init_state ()) (Scrabble.get_init_state ()) 
        (State.get_init_state ())
    end
  | _ -> main ()

(* Execute the game engine. *)
let () = main ()


