open Scrabble

type object_phrase = string list
type command = 
  (* | Cell of Scrabble.grid
     | Tile of Scrabble.tile *)
  | Cell of object_phrase
  | Tile of object_phrase
  | Check
  | Valid
  | Invalid

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

(* return true or false *)
(* let valid_cell cell =
   let board = Scrabble.board in *)
(* 
let valid_play cell tile =
if both are valid -> update board -> return board *)

(** [eliminate_empty lst acc] is the list [lst] with all empty strings 
    removed. *)
let rec eliminate_empty lst acc=
  match lst with 
  | [] -> acc
  | h::t -> if h = " " || h = "" then eliminate_empty t acc
    else eliminate_empty t (h::acc)

(** [parse str] is the list [lst] with all empty strings 
    removed. *)
let parse str =
  let parsed_str = String.split_on_char ' ' str in
  let cleaned_lst = List.rev (eliminate_empty parsed_str []) in 
  if cleaned_lst = [] || str = "" then raise Empty
  else
    match cleaned_lst with
    | [] -> raise Empty
    | h::t -> if h = "cell" && (List.length t <> 0) then Cell t 
      else if h = "tile" && (List.length t <> 0) then Tile t 
      else if h = "check" && (List.length t = 0) then Check
      else if h = "valid" && (List.length t = 0) then Valid
      else if h = "invalid" && (List.length t = 0) then Invalid
      else raise Malformed