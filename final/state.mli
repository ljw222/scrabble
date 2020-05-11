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

(** [get_init_state ()] is the initial state *)
val get_init_state: unit -> t

(** [player_turn state] is the current player who's turn it is in of [state] *)
val player_turn: t -> Scrabble.players

(** [winning_score state] is the winning score of [state] *)
val winning_score: t -> int