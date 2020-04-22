(* Will have all the set up of the types, similar to adventure.ml*) 
(* Consider the layout of the whole game, not just the board itself *)

(** value of x and y [0:10] included*)
type x = int
type y = int 
type grid= (x*y)

type location_id = 
  |Hand 
  |Board of grid
  |Bag 

type tile = {
  id: int;
  letter: char;
  point: int;
  location: location_id;
}
type bag = {
  contents: tile list;
}

type contents_option = 
  | Some of tile
  | None

(* type board_cell = {
   grid : (x*y);
   bonus: int;
   tile : contents_option;
   } *)
type board ={
  (* grid : grid; *)
  (* contents : board_cell list; *)
  (* bonus_grid : (grid * int); *)

  cells : (grid * contents_option) list;
  point_bonus : (grid * int) list;
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
  (* location: location_id;
     tiles: tile list ;
     turn: players; *)
  board: board;
  players: players list;
}


(* let bag = [("a";1);("b";2)] *)
