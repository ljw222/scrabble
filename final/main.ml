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
  print_endline("");
  Scrabble.print_board current_game.board;
  print_endline("");
  Scrabble.print_hand player current_game;
  print_endline("");
  print_string("Enter 'cell (_,_)' to pick a cell to play");
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
      begin
        check_helper start_of_turn_game current_game player
      end
    |_ -> failwith ""
  with _ -> 
    print_endline("");
    print_string "This is not a valid command. Try again";
    print_endline("");
    play_game start_of_turn_game current_game player

and check_helper start_of_turn_game current_game player =
  if Scrabble.check_if_valid start_of_turn_game current_game then (
    print_endline("");
    print_string "Other player please confirm words";
    print_endline("");
    Scrabble.print_words start_of_turn_game current_game;
    print_endline("");
    match Command.parse (read_line ()) with
    | Valid -> if (player = Scrabble.Player1 (Scrabble.get_init_player1 ()))
      then (print_endline("");print_endline("Switch player!");
            play_game current_game (Scrabble.refill_hand current_game player) 
              (Scrabble.Player2 (Scrabble.get_init_player2 ())))
      else (print_endline("");print_endline("Switch player!");
            play_game current_game (Scrabble.refill_hand current_game player) 
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
                  "\n\nWelcome to our Scrabble Inspired Game!\n");
  ANSITerminal.(print_string [blue]
                  "\nType 'start' to begin!\n");
  print_string  "> ";
  match read_line () with
  | "start" -> 
    play_game (Scrabble.get_init_state ()) (Scrabble.get_init_state ()) 
      (State.player_turn (State.get_init_state ()))
  | _ -> main ()

(* Execute the game engine. *)
let () = main ()


