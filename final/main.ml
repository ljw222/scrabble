open Scrabble
open Command
open State

(* how to return back to tile match smnt if tile command isnt found *)
(* tell that tile letter need to be in caps, cells need to be type (_,_) *)

(* LATER: *)
(* points *)
(* checking when game is won *)
(* score function / display score *)
(* bonus points tile*)
(* if no words can be made, switch to new hand tiles -> if no words can be made then switch entire hand *)
(* print tiles in different color (when placing during turn) *)

(* BUGS *)
(* you can remove any tile, not just ones placed in your turn *)
(* you have to type cell inorder to place tile if type tile incorrectly *)
(* if valid/invalid is spelled wrong then just goes to player2's turn *)

(* TONIGHT: *)
(* once done command is executed, should check if the tiles make correct 
   words/are connected to existing words --> if not then remove all tiles 
   from that turn*)
let basic_info start_of_turn_game current_game player = 
  let current_player = State.player_turn player in 
  let player_type = 
    match current_player with
    | Player1 _ -> "player1"
    | Player2 _ -> "player2" in 
  print_endline("Current Player: " ^ player_type);
  let player_score = Int.to_string (State.get_player_score player_type player) in
  print_endline("Score as of last turn:" ^ player_score);
  print_endline("");
  Scrabble.print_board current_game.board;
  print_endline("");
  Scrabble.print_hand current_player current_game;
  print_endline("");
  print_string("Enter 'cell (_,_)' to pick a cell to play");
  print_endline("")

let rec play_game start_of_turn_game current_game player =
  print_endline("_____________________________");
  let current_player = State.player_turn player in 
  basic_info start_of_turn_game current_game player;
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
          print_endline("line 56");
          let new_game_state = 
            Scrabble.play (first,second) (String.get (List.nth t 0) 0) current_game current_player 
          in
          play_game start_of_turn_game new_game_state player
        | Remove -> 
          let first = (Char.code (String.get (List.nth c 0) 1)) - 48 in 
          let second = (Char.code (String.get (List.nth c 0) 3)) - 48 in 
          print_endline("line 64");
          let new_game_state = Scrabble.delete (first,second) current_game current_player in 
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
    if new_score >= State.winning_score player then
      begin
        print_endline("[Player __] won the game! yay!");
        exit 0
      end
    else
      match Command.parse (read_line ()) with
      | Valid -> 
        if (current_player_type = "player1")
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
    play_game (Scrabble.get_init_state ()) (Scrabble.get_init_state ()) (State.get_init_state ())
  (* (State.player_turn (State.get_init_state ())) *)
  | _ -> main ()

(* Execute the game engine. *)
let () = main ()


