(* open Scrabble *)
type game_state = 
  | Ongoing
  | Done
type t = {
  winning_score: int;
  state: game_state;
  player_turn: Scrabble.players;
  (* board: Scrabble.board;
     all_tiles: Scrabble.tile list; *)
}

let init_state = {
  winning_score = 20;
  state = Ongoing;
  player_turn = Scrabble.Player1 (Scrabble.get_init_player1 ())
  (* List.nth (scrabble_init.players) 0; *)
  (* board = scrabble_init.board;
     all_tiles = scrabble_init.all_tiles; *)
}

let get_init_state () =  
  init_state

let player_turn state =
  state.player_turn