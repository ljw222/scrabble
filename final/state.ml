(* open Scrabble *)
type game_state = 
  | Ongoing
  | Done
type t = {
  winning_score: int;
  state: game_state;
  player_turn: Scrabble.players;
  player1: Scrabble.players;
  player2: Scrabble.players;
}

let init_state = {
  winning_score = 20;
  state = Ongoing;
  player_turn = Scrabble.Player1 (Scrabble.get_init_player1 ());
  player1 = Scrabble.Player1 (Scrabble.get_init_player1 ());
  player2 = Scrabble.Player2 (Scrabble.get_init_player2 ())
}

let get_init_state () =  
  init_state

let player_turn state =
  state.player_turn

let update_player1 state new_score new_player1 = 
  {
    winning_score = state.winning_score;
    state = state.state;
    player_turn = state.player2;
    player1 = new_player1;
    player2 = state.player2
  }

let update_player2 state new_score new_player2 = 
  {
    winning_score = state.winning_score;
    state = state.state;
    player_turn = state.player1;
    player1 = state.player1;
    player2 = new_player2
  }

let get_player_score player_type state = 
  Scrabble.player_score state.player1 player_type
