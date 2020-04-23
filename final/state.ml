(* Note: You may introduce new code anywhere in this file. *) 
open Scrabble
(* TODO: replace [unit] with a type of your own design. *)
type game_state = 
  | Ongoing
  | Done

type t = {
  winning_score: int;
  state: game_state;
  player_turn: Board.players;
  board: Board.board;
}


