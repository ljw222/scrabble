open Scrabble
open Command
open State

(* how to return back to tile match smnt if tile command isnt found *)
(* tell that tile letter need to be in caps, cells need to be type (_,_) *)

(* LATER: *)
(* remove command *)
(* points *)
(* changing players *)
(* checking when game is won *)
(* other player checking if they agree with words made (command? yes/no) *)

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
      print_endline("");
      print_string "Player 2 please confirm words";
      print_endline("");
      (* play_game (Scrabble.refill_hand current_game player) start_of_turn_game player *)
      if Scrabble.check_if_valid start_of_turn_game current_game then print_endline("true")
      else print_endline("false")

  (* match Scrabble.check_if_valid start_of_turn_game current_game with
     | true -> print_endline("true")
     | false -> print_endline("false")
     | _ -> print_endline("not allowed") *)
  (* change players *)
  with _ -> 
    print_endline("");
    print_string "This is not a valid command. Try again";
    print_endline("");
    play_game start_of_turn_game current_game player


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


