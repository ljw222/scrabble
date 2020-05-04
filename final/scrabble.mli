type x = int
type y = int 
type grid= (x*y)
type score = int
exception Invalid_Play

type player = {
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
  letter: Char.t;
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

val valid_cell: grid -> board -> bool

val char_in_bag: tile list -> Char.t -> bool

val valid_tile: Char.t -> tile list -> bool

val update_tile_loc: Char.t -> tile list -> grid -> tile list -> tile list

val tile_to_update: tile list -> Char.t -> tile

val update_board_cells: ('a * contents_option) list ->
  tile -> 'a -> ('a * contents_option) list -> ('a * contents_option) list

val play: grid -> Char.t -> t -> t

val print_board: ('a * contents_option) list -> int -> int -> int -> unit

val print_init_board: 'a -> unit

val get_board: t -> board