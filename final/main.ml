open Scrabble
open Command

(* how to return back to tile match smnt if tile command isnt found *)
(* tell that tile letter need to be in caps, cells need to be type (_,_) *)

let rec play_game game =
  Scrabble.print_board game.board;
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
          let new_game_state = Scrabble.play (first,second) (String.get (List.nth t 0) 0) game in
          play_game new_game_state
        | _ -> 
          print_endline("");
          print_string "Please enter a tile command. Try again";
          print_endline("");
          play_game game
      end
    | Tile t -> 
      print_endline("");
      print_string "You must enter a cell location first. Try again";
      print_endline("");
      play_game game
    | Done -> print_string "done";
  with _ -> 
    print_endline("");
    print_string "This is not a valid command. Try again";
    print_endline("");
    play_game game


(* * [start_game] starts the game.
   let start_game f =
   Scrabble.print_init_board ();
   print_string("Enter 'Cell [coordinate grid]' to pick a cell to play");
   print_newline();
   (* play_game Scrabble.init_board (read_line ()) *)
   match read_line () with
   | "start" -> play_game ()
   | _ -> failwith "" *)

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to our Scrabble Inspired Game!\n");
  ANSITerminal.(print_string [blue]
                  "\nType 'start' to begin!\n");
  print_string  "> ";
  match read_line () with
  | "start" -> play_game (Scrabble.get_init_state ())
  | _ -> main ()

(* Execute the game engine. *)
let () = main ()

(* open Scrabble
   open Command

   (* * [play_game] starts the game. *)
   let play_game f =
   Scrabble.print_init_board ()
   (* match read_line () with
   | "start" -> play_game ()
   | _ -> main () *)

   (** [main ()] prompts for the game to play, then starts it. *)
   let rec main () =
   ANSITerminal.(print_string [blue]
                  "\n\nWelcome to our Scrabble Inspired Game!\n");
   ANSITerminal.(print_string [blue]
                  "\nType 'start' to begin!\n");
   print_string  "> ";
   match read_line () with
   | "start" -> play_game ()
   | _ -> main ()

   (* Execute the game engine. *)
   let () = main () *)


