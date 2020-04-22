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

let all_tiles = [('A',1); ('B',3); ('C',3); ('D',2); ('E',1); ('F',4); ('G',2); 
                 ('H',4); ('I',1); ('J',8); ('K',5); ('L',1); ('M',3); ('N',1); 
                 ('O',1); ('P',3); ('Q',10);('R',1); ('S',1); ('T',1); ('U',1); 
                 ('V',4); ('W',4); ('X',8); ('Y',4); ('Z',10)] 

let rec create_init_tiles letter_points acc counter =
  match letter_points with
  | [] -> acc
  | h::t -> let new_tile = 
              {id=counter;
               letter=fst h;
               point=snd h;
               location=Bag;} in create_init_tiles t (new_tile::acc) (counter+1)

let init_tiles = 
  create_init_tiles (all_tiles@all_tiles@all_tiles@all_tiles) [] 0

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
