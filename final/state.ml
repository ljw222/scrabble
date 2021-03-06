type game_state = 
  | Ongoing
  | Done
type t = {
  winning_score: int;
  state: game_state;
  player_turn: Scrabble.players;
}

let init_state = {
  (* this score can be changed, which may make testing easier *)
  winning_score = 50;
  state = Ongoing;
  player_turn = Scrabble.Player1 (Scrabble.get_init_player1 ())
}

let get_init_state () =  
  init_state

let player_turn state =
  state.player_turn

let winning_score state = 
  state.winning_score