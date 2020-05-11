(** 
   Representation of dynamic scrabble game state.

   This module represents the state of the scrabble game as two players play 
   tthe game. Information such as the score of a player and players 
   turn may cause the state to change.
*)

type t 

type game_state = 
  | Ongoing
  | Done

val get_init_state: unit -> t

val player_turn: t -> Scrabble.players

val winning_score: t -> int