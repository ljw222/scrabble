open Scrabble
open Command

(* * [play_game] starts the game. *)
let play_game f =
  (* let board = Scrabble.init_state.board *)
  failwith "unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to our Scrabble Inspired Game!\n");
  ANSITerminal.(print_string [blue]
                  "\nType 'start' to begin!\n");
  print_string  "> ";
  match read_line () with
  | "start" -> play_game 1
  | _ -> main ()

(* Execute the game engine. *)
let () = main ()

