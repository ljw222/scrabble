(* open Scrabble *)
type game_state = 
  | Ongoing
  | Done
type t = {
  winning_score: int;
  state: game_state;
  player_turn: Scrabble.players;
}

let init_state = {
  winning_score = 50;
  state = Ongoing;
  player_turn = Scrabble.Player1 (Scrabble.get_init_player1 ())
}

(** [get_init_state ()] is the initial state *)
let get_init_state () =  
  init_state

(** [player_turn state] is the current player who's turn it is in of [state] *)
let player_turn state =
  state.player_turn

(** [winning_score state] is the winning score of [state] *)
let winning_score state = 
  state.winning_score