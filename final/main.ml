open Scrabble
(* open Command *)

(* * [play_game f] starts the adventure in file [f]. *)
let play_game f =
  failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to our Scrabble Inspired Game!\n");
  (* print_endline "Please enter the name of the game file you want to load.\n"; *)
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()

