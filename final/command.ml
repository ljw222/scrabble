open Scrabble

type command = 
  | Cell of Scrabble.grid
  | Tile of Scrabble.tile
  | Done

(* Cell (1,1)
   have function that determines if cell is avail on board
   Tile A
   have function that determines if tile is avail in bag
   function that updates the board and the tiles

   function that takes in cell and tile and updates the state 
   (board and tiles) *)

type result = Legal of t | Illegal

exception Empty

exception Malformed

type t = string
(* return true or false *)
(* let valid_cell cell =
   let board = Scrabble.board in *)
(* 
let valid_play cell tile =
if both are valid -> update board -> return board *)