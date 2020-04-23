(* Will have all the set up of the types, similar to adventure.ml*) 
(* Consider the layout of the whole game, not just the board itself *)

(** value of x and y [0:10] included*)
type x = int
type y = int 
type grid= (x*y)

type player = {
  (* hand: tile list; *)
  score: int;
}

type players = 
  |Player1 of player
  |Player2 of player

type location_id = 
  |Hand of players
  |Board of grid
  |Bag 

type tile = {
  id: int;
  letter: char;
  point: int;
  location: location_id;
}

type contents_option = 
  | Some of tile
  | None

type board = {
  cells : (grid * contents_option) list;
  point_bonus : (grid * int) list;
}

type t = {
  all_tiles: tile list;
  board: board;
  players: players list;
}

let init_player1 = {score=0}

let char_tiles = [('A',1); ('B',3); ('C',3); ('D',2); ('E',1); ('F',4); ('G',2); 
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

let starting_tile = {id=0; letter='A'; point=1; location=Bag }
let all_tiles = 
  starting_tile::(create_init_tiles (char_tiles@char_tiles@char_tiles@char_tiles) [] 1)

(** [location_tile tiles location acc] is the list of tiles from [tiles] 
    that are located in [location]*)
let rec location_tile tiles location acc =
  match tiles with
  | [] -> acc
  | h::t -> 
    if h.location = location then location_tile t location (h::acc) else 
      location_tile t location acc

(** [choose_tile tiles player] all tiles in [tiles] with a random tile location 
    changed to be in the hand of [player] *)
let choose_tile tiles player =
  let bag_tiles = location_tile tiles Bag [] in
  let rand = Random.int((List.length bag_tiles) - 1) in
  let tile = List.nth tiles rand in
  let new_tile = 
    {id=tile.id; letter=tile.letter; point= tile.point; location=Hand player} in
  List.map (fun old_tile -> 
      if old_tile.id = new_tile.id then new_tile else old_tile) tiles

(** [choose_tile tiles player] all tiles in [tiles] with a random tile chose as 
    the starting tile *)
let choose_first_tile tiles frst_tile =
  frst_tile::tiles

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
  if x = 6 && y = 6 then create_init_bonus2 acc (x-1) (y+1)
  else if x >= 0 && y >= 0 then create_init_bonus2 (((x,y),2)::acc) (x-1) (y+1)
  else acc

let empty_cells = create_init_board [] 10 10

let init_board = {
  cells = ((6,6), Some starting_tile)::empty_cells;
  point_bonus = (create_init_bonus1 [] 10 10)@(create_init_bonus2 [] 10 0);
}

let init_state = {
  all_tiles = all_tiles;
  board = init_board;
  players = [Player1 init_player1];
}