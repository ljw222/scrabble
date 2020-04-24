(* Note: You may introduce new code anywhere in this file. *) 
open Scrabble
(* TODO: replace [unit] with a type of your own design. *)
type game_state = 
  | Ongoing
  | Done

(* type t = string *)
type t = {
  winning_score: int;
  state: game_state;
  player_turn: Scrabble.players;
  board: Scrabble.board;
  all_tiles: Scrabble.tile list;
}

let init_state scrabble_init = {
  winning_score = 20;
  state = Ongoing;
  player_turn = 
    (* Scrabble.(Player1 init_player1); *)
    List.nth (scrabble_init.players) 0;
  board = scrabble_init.board;
  all_tiles = scrabble_init.all_tiles;
}