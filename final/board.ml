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

let rec create_init_tiles tiles_points acc counter =
  match tiles_points with
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

let init_bag = {contents = init_tiles}

type contents_option = 
  | Some of tile
  | None

type board = {
  cells : (grid * contents_option) list;
  point_bonus : (grid * int) list;
}

let rec create_init_board acc x y = 
  match x,y with
  | 0,0 -> acc
  | x,y ->
    begin
      if y = 0 then create_init_board (((x,y), None)::acc) (x-1) (10) else
        match x,y with
        | x,y -> 
          if (y = 0 && x != 0) then create_init_board (((x,y), None)::acc) (x-1) (y)
          else if (y != 0 && x < 10) then create_init_board (((x,y), None)::acc) (x) (y-1)
          else create_init_board (((x,y), None)::acc) (x-1) (10)
    end

let rec create_init_bonus1 acc x y = 
  if x >= 0 && y >= 0 then create_init_bonus1 (((x,y),2)::acc) (x-1) (y-1)
  else acc

let rec create_init_bonus2 acc x y = 
  if x = 5 && y = 5 then create_init_bonus2 acc (x-1) (y+1)
  else if x >= 0 && y >= 0 then create_init_bonus2 (((x,y),2)::acc) (x-1) (y+1)
  else acc

let init_board = {
  cells = create_init_board [] 10 10;
  point_bonus = (create_init_bonus1 [] 10 10)@(create_init_bonus2 [] 10 0);
}
type player = {
  hand: tile list;
  score: int;
}

type players = 
  |Player1 of player
  |Player2 of player

type t = {
  bag: tile list;
  board: board;
  players: players list;
}
