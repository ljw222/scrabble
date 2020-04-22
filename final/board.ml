(* Will have all the set up of the types, similar to adventure.ml*) 
(* Consider the layout of the whole game, not just the board itself *)

(** value of x and y [0:10] included*)
type x = int
type y = int 
type grid= (x*y)


type board ={
  grid : grid;
  bonus_grid : (grid * int)
}


type location_id = 
  |Hand 
  |Board of board
  |Bag 

type tile = {
  letter: char;
  point: int;
  start_loc: location_id;
}
(** tuple that has location as key and letter at the grid as value*)
(* 
type on_grid = 
  | Some of grid 
  | None *)

(* tile*location list *)
(** tuple that has location as key and d as value*)
(* 
type board_info = {
  tiles_contained: (tile*on_grid) list;
  bonus: grid_bonus list;
} *)

type player = {
  hand: tile list;
  score: int;
}

type players = 
  |Player1 of player
  |Player2 of player

type t = {
  location: location_id;
  tiles: tile list ;
  turn: players;
}
