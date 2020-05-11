(** 
   Representation of the commands that a player may play during 
   an ongoing scrabble game. Cell of game_phrase, Tile or game_phrase, 
   Remove, Check< Valid, Invalid, QUit, and Stuck are all actions that the 
   player may need during their time playing the game.

   The type [game_phrase] represents the game action play that a player wants 
   to play. Each element of the list represents a move such as cell or tile 
   letter. An [game_phrase] is not permitted to be the empty list, as this 
   would mean that no move specification is given.
*)

type game_phrase = string list
type command = 
  | Cell of game_phrase
  | Tile of game_phrase
  | Remove
  | Check
  | Valid
  | Invalid
  | Quit
  | Stuck

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] is the list [lst] with all empty strings removed. *)
val parse : string -> command