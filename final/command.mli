type object_phrase = string list
type command = 
  (* | Cell of Scrabble.grid
     | Tile of Scrabble.tile *)
  | Cell of object_phrase
  | Tile of object_phrase
  | Check

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

val parse : string -> command