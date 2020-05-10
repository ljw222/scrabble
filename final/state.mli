(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing the game state. *)
type t 

type game_state = 
  | Ongoing
  | Done

val get_init_state: unit -> t

val player_turn: t -> Scrabble.players

val update_player1: t -> Scrabble.players -> t

val update_player2: t -> Scrabble.players -> t

val get_player_score: string -> t -> int

val winning_score: t -> int