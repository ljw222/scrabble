(** 
   Representation of dynamic scrabble state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.

   This module represents the information and rules required for a scrabble 
   game as it is being played, including the points and tiles availible, 
   location of tiles, points of each player. and whether moves played are 
   valid (i.e. follow the rules of our game)
*)

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

(** [valid_cell cell board] is true if the [cell] is valid/availible [board] *)
val valid_cell: grid -> board -> bool

(** [char_in_collection collection char] is true if a tile with the letter 
    [char] is in tile collection [collection]. otherwise false *)
val char_in_collection: tile list -> Char.t -> bool

(** [valid_tile tile all_tiles] is true if [tile] is available in the bag of 
    [all_tiles]. Otherwise false *)
val valid_tile: Char.t -> tile list -> bool

(** [update_tile_loc tile_letter all_tiles cell acc] updates [all_tiles] with 
    the tile in the bag with letter [tile_letter] to be in the board *)
val update_tile_loc: Char.t -> tile list -> grid -> tile list -> tile list

(** [tile_to_update tiles_in_bag tile_letter] is the first tile in 
    [tiles_in_bag] that has the letter [tile_letter]. 
    Requires: [tiles_in_bag] contains a tile with letter [tile_letter] *)
val tile_to_update: tile list -> Char.t -> tile

(** [update_board_cells board_cells tile cell acc] updates contents of [cell]
    in [board_cells] to contain [tile]*)
val update_board_cells: (grid * contents_option) list ->
  tile -> grid -> (grid * contents_option) list -> (grid * contents_option) list

(** [play cell tile_letter state] if the state when tile with [tile_letter] is 
    put in [cell] given current state [state] *)
val play: grid -> Char.t -> t -> players -> t

(** [print_board board_cells acc x y] prints [board_cells] *)
val print_board: ('a * contents_option) list -> int -> int -> int -> unit

(** [print_board ()] prints the initial board cells *)
val print_init_board: unit -> unit

(** [print_board board] prints the board cells of [board] *)
val print_board: board -> unit

(** [get_init_state ()] the initial state of the game *)
val get_init_state: unit -> t

(** [get_init_player1 ()] gets the initial player1 *)
val get_init_player1: unit -> player

(** [get_init_player2 ()] gets the initial player2 *)
val get_init_player2: unit -> player

(** [print_hand player game_state] prints the hand of [player] *)
val print_hand: players -> t -> unit

(** [refill_hand state player] refills hand of [player] to 7 tiles after turn is
    a valid check *)
val refill_hand: t -> players -> int -> t

(** [check_if_valid beg_state end_state] is true if the turn spanning from 
    [beg_state] to [end_state] consists of valid moves. Otherwise false.*)
val check_if_valid: t -> t -> bool

(** [print_words new_words beg_state end_state] prints all new words made from
    [beg_state] to [end_state] *)
val print_words: t -> t -> unit

(** [return_current_score player] is the score of [player] *)
val return_current_score: players -> int

(** [points_of_turn beg_state end_state] points of all words played in turn 
    starting at [beg_state] and ending in [end_state] *)
val points_of_turn: t -> t -> int

(** [update_player1 new_score current_state acc] updates player1's score in 
    [current_state] to be [new_score] *)
val update_player: string -> int -> players

(** [update_state new_score current_state player] updates [current_state] when 
    [player]'s score changes to [new_score]*)
val update_state: int -> t -> players -> t

(** [return_current_score2 player_type state ] is the score of [player_type] 
    in game state [state] *)
val return_current_score2: string -> t -> int

(** [delete cell state player] removes tile in [cell] from board in [state] *) 
val delete: grid -> t -> players -> t -> t -> t

(** [occupied_cell cell board] is true if the [cell] contains a tile *)
val occupied_cell: grid -> board -> bool

(** [valid_tile_in_hand tile_letter all_tiles player] is true if [tile] is 
    available in the hand of [player]. Otherwise false *)
val valid_tile_in_hand: Char.t -> tile list -> players -> bool

(** [tile_hand_to_board tile_letter all_tiles cell acc player] updates 
    [all_tiles] with the tile in the hand of [player] with letter [tile_letter] 
    to be in the board *)
val tile_hand_to_board: 
  Char.t -> tile list -> grid -> tile list -> players -> tile list

(** [string_of_tiles hand_tiles] string of list [hand_tiles] *)
val string_of_tiles: tile list -> string

(** [list_diff l1 l2 acc] is the difference between [l1] and [l2] *)
val list_diff: 'a list -> 'a list -> 'a list -> 'a list

(** [loc_in_tiles tiles grid_loc] is true if there is a tile in [tiles] with 
    board location [grid_loc]. Otherwise false *)
val loc_in_tiles: tile list -> grid -> bool

(** [tiles_in_player_hand tiles player_type acc] is the list of tiles from
    [tiles] that are located in [player_type hand]*)
val tiles_in_player_hand: tile list -> string -> tile list -> tile list
