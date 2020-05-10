open Scrabble
open Command
open State

(* how to return back to tile match smnt if tile command isnt found *)
(* tell that tile letter need to be in caps, cells need to be type (_,_) *)

(* LATER: *)
(* points *)
(* fucntion that returns all of the new words made *)
(* checking when game is won *)
(* score function / display score *)
(* bonus points tile*)
(* if no words can be made, switch to new hand tiles -> if no words can be made then switch entire hand *)

(* BUGS *)
(* you can remove any tile, not just ones placed in your turn *)
(* you have to type cell inorder to place tile if type tile incorrectly *)
(* if valid/invalid is spelled wrong then just goes to player2's turn *)

(* TONIGHT: *)
(* once done command is executed, should check if the tiles make correct 
   words/are connected to existing words --> if not then remove all tiles 
   from that turn*)
(* fucntion that returns all of the new words made *)
(* switching players *)

let rec play_game start_of_turn_game current_game player =
  let player_type = 
    match player with
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in 
  print_endline("Current Player: " ^ player_type);
  (* let current_score = Scrabble.return_current_score player in *)
  let current_score2 = Scrabble.return_current_score2 player_type current_game in
  print_endline("Score as of beginning of turn:" ^ string_of_int(current_score2));
  print_endline("");
  Scrabble.print_board current_game.board;
  print_endline("");
  Scrabble.print_hand player current_game;
  print_endline("");
  ANSITerminal.(print_string [blue]
                  "\nEnter 'cell (_,_)' to pick a cell to play\n");
  print_endline("");
  try 
    match Command.parse (read_line ()) with
    | Cell c -> 
      begin
        print_string "Enter 'tile _'";
        print_endline("");
        match Command.parse (read_line ()) with
        | Tile t -> 
          let first = (Char.code (String.get (List.nth c 0) 1)) - 48 in 
          let second = (Char.code (String.get (List.nth c 0) 3)) - 48 in 
          let new_game_state = 
            Scrabble.play (first,second) (String.get (List.nth t 0) 0) current_game player 
          in
          play_game start_of_turn_game new_game_state player
        | Remove -> 
          let first = (Char.code (String.get (List.nth c 0) 1)) - 48 in 
          let second = (Char.code (String.get (List.nth c 0) 3)) - 48 in 
          let new_game_state = Scrabble.delete (first,second) current_game player in 
          play_game start_of_turn_game new_game_state player
        | Quit -> print_endline "Thanks for playing!"
        | _ -> 
          print_endline("");
          print_string "Please enter a tile command. Try again";
          print_endline("");
          play_game start_of_turn_game current_game player
      end
    | Tile t -> 
      print_endline("");
      print_string "You must enter a cell location first. Try again";
      print_endline("");
      play_game start_of_turn_game current_game player

    | Check -> 
      check_helper start_of_turn_game current_game player
    | Quit -> print_endline "Thanks for playing!";
    | Stuck -> 
      print_endline("");
      print_string "Your turn is skipped";
      print_endline("");
      print_endline("Switch player!");
      if (player = Scrabble.Player1 (Scrabble.get_init_player1 ()))
      then play_game start_of_turn_game current_game 
          (Scrabble.Player2 (Scrabble.get_init_player2 ()))
      else play_game start_of_turn_game current_game
          (Scrabble.Player1 (Scrabble.get_init_player1 ()))
    |_ -> failwith ""
  with _ -> 
    print_endline("");
    print_string "This is not a valid command. Try again";
    print_endline("");
    play_game start_of_turn_game current_game player

and check_helper start_of_turn_game current_game player =
  let player_type = 
    match player with
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in 
  let current_score2 = Scrabble.return_current_score2 player_type current_game in
  let turn_score = Scrabble.points_of_turn start_of_turn_game current_game in
  let new_score = current_score2 + turn_score in
  let player_type = 
    match player with
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in 
  if Scrabble.check_if_valid start_of_turn_game current_game then (
    print_endline("");
    print_string "Other player please confirm words";
    print_endline("");
    Scrabble.print_words start_of_turn_game current_game;
    print_endline("");
    (* let new_score = 
       Scrabble.points_of_turn start_of_turn_game current_game in *)
    match Command.parse (read_line ()) with
    | Valid -> if new_score >= State.winning_score (get_init_state ()) then
        begin
          print_endline(player_type ^ " won the game! Yay!");
          exit 0
        end
      else if (player_type = "player1")
      then (
        (* stores the current score of player 1 *)
        (* let current_score = Scrabble.return_current_score player in *)
        (* let current_score = Scrabble.return_current_score2 player_type current_game in
           let turn_score = Scrabble.points_of_turn start_of_turn_game current_game in
           let new_score = current_score + turn_score in *)
        (* let new_players = Scrabble.update_player "player1" new_score in *)
        print_endline("");
        print_endline("Switch player!");
        play_game current_game (Scrabble.refill_hand current_game player new_score) 
          (Scrabble.Player2 (Scrabble.get_init_player2 ())))
      else (
        (* let current_score = Scrabble.return_current_score player in *)
        (* let current_score = Scrabble.return_current_score2 player_type current_game in
           let turn_score = Scrabble.points_of_turn start_of_turn_game current_game in
           let new_score = current_score + turn_score in *)
        (* let new_players = Scrabble.update_player "player2" new_score in *)
        print_endline("");
        print_endline("Switch player!");
        play_game current_game (Scrabble.refill_hand current_game player new_score) 
          (Scrabble.Player1 (Scrabble.get_init_player1 ())))
    | Invalid -> print_endline("word not valid");
      play_game start_of_turn_game current_game player
    | Quit -> print_endline "Thanks for playing!"
    | _ -> check_helper start_of_turn_game current_game player
  )
  else (print_endline("");
        print_string "Please enter a valid move. Try again";
        print_endline("");
        play_game start_of_turn_game current_game player)

(* check which player is currently playing (from the Scrabble.t list) *)
(* and validate_check start_of_turn_game current_game player= 
   match Command.parse (read_line ()) with
   | Valid -> if (player = Scrabble.Player1 (Scrabble.get_init_player1 ()))
   then (play_game current_game current_game (Scrabble.Player2 (Scrabble.get_init_player2 ())))
   else play_game start_of_turn_game current_game player
   | Invalid -> print_endline("word not valid");play_game start_of_turn_game current_game player
   | _ -> failwith "type valid/invalid" *)
(* make sure to remove all the new tiles from the board to the hand *)

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [blue]
                  "\nWelcome to our Scrabble Inspired Game!\n");
  ANSITerminal.(print_string [blue]
                  "\nHere are some rules with examples:\n");
  ANSITerminal.(print_string [white]
                  "\n1. Place a tile on the board \n");
  ANSITerminal.(print_string [green]
                  "cell (1,0)\n");
  ANSITerminal.(print_string [green]
                  "tile A\n");
  ANSITerminal.(print_string [white]
                  "2. Remove a tile from the board\n");
  ANSITerminal.(print_string [green]
                  "cell (1,0)\n");
  ANSITerminal.(print_string [green]
                  "remove\n");
  ANSITerminal.(print_string [white]
                  "3. Finish your turn by asking the other player to validate your input\n");
  ANSITerminal.(print_string [green]
                  "check\n");   
  ANSITerminal.(print_string [white]
                  "4. When checking the words made of another player\n");
  ANSITerminal.(print_string [green]
                  "valid or invalid\n");
  ANSITerminal.(print_string [white]
                  "5. Quit the game anytime with command\n");
  ANSITerminal.(print_string [green]
                  "quit\n");
  ANSITerminal.(print_string [white]
                  "6. If you don't have a move and want to skip your turn\n");
  ANSITerminal.(print_string [green]
                  "stuck\n");
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


