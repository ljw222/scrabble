type x = int
type y = int 
type grid= (x*y)
type score = int
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

let init_player1 = {score=0}
let init_player2 = {score=0}

let get_init_player1 () =
  init_player1

let get_init_player2 () =
  init_player2

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

let starting_tile = {id=0; letter='A'; point=1; location=Board (0,0)}
(* all tiles in bag with starting_tile on board *)
let all_tiles = 
  starting_tile::(create_init_tiles (char_tiles@char_tiles@char_tiles@char_tiles) [] 1)
(* (create_init_tiles (char_tiles@char_tiles@char_tiles@char_tiles) [] 1) *)

(** [location_tile tiles location acc] is the list of tiles from [tiles] 
    that are located in [location]*)
let rec location_tile tiles location acc =
  match tiles with
  | [] -> List.rev acc
  | h::t -> 
    if h.location = location then location_tile t location (h::acc) else 
      location_tile t location acc

(** [choose_tile tiles player num_tiles] all tiles in [tiles] with a random tile location 
    changed to be in the hand of [player] [num_tiles] times *)
let rec choose_tile tiles player num_tiles =
  if num_tiles = 0 then tiles 
  else
    let bag_tiles = location_tile tiles Bag [] in
    let rand = Random.int((List.length bag_tiles) - 1) in
    let tile = List.nth tiles rand in
    let new_tile = 
      {id=tile.id; letter=tile.letter; point= tile.point; location=Hand player} in
    let updated_tiles = List.map (fun old_tile -> 
        if old_tile.id = new_tile.id then new_tile else old_tile) tiles in
    choose_tile updated_tiles player (num_tiles - 1)

(* deal 7 tiles to player1 *)
let init_tiles_player1 = 
  choose_tile all_tiles (Player1 init_player1) 8
(* deal 7 tiles to player2 *)
let init_tiles_player2 = 
  choose_tile init_tiles_player1 (Player2 init_player1) 8

let rec create_init_board acc x y = 
  match x,y with
  | 0,0 -> acc
  | x,y ->
    begin
      if y = 0 then create_init_board (((x,y), None)::acc) (x-1) (9) else
        match x,y with
        | x,y -> 
          if (y = 0 && x != 0) then create_init_board (((x,y), None)::acc) (x-1) (y)
          else if (y != 0 && x <= 9) then create_init_board (((x,y), None)::acc) (x) (y-1)
          else create_init_board (((x,y), None)::acc) (x-1) (9)
    end

let rec create_init_bonus1 acc x y = 
  if x >= 0 && y >= 0 then create_init_bonus1 (((x,y),2)::acc) (x-1) (y-1)
  else acc

let rec create_init_bonus2 acc x y = 
  if x = 6 && y = 6 then create_init_bonus2 acc (x-1) (y+1)
  else if x >= 0 && y >= 0 then create_init_bonus2 (((x,y),2)::acc) (x-1) (y+1)
  else acc

let empty_cells = create_init_board [] 9 9

let init_board = {
  cells = ((0,0), Some starting_tile)::empty_cells;
  point_bonus = (create_init_bonus1 [] 9 9)@(create_init_bonus2 [] 9 0);
}

let init_state = {
  all_tiles = init_tiles_player2;
  board = init_board;
  players = [Player1 init_player1;Player2 init_player2];
}

(** [valid_cell cell board] is true if the [cell] is valid/availible [board] *)
let valid_cell cell board = 
  let valid_grid_params = List.mem_assoc cell board.cells in 
  if not valid_grid_params then false
  else 
    let avail_grid = List.assoc cell board.cells in 
    avail_grid = None

(** [char_in_collection collection char] is true if a tile with the letter [char] is in 
    tile collection [collection]. otherwise false *)
let rec char_in_collection collection char = 
  match collection with
  | [] -> false
  | h::t -> if h.letter = char then true else char_in_collection t char

(** [valid_tile tile all_tiles] is true if [tile] is available in the bag of 
    [all_tiles]. Otherwise false *)
let valid_tile tile_letter all_tiles =
  let bag_tiles = location_tile all_tiles Bag [] in
  char_in_collection bag_tiles tile_letter 

(** [valid_tile_in_hand tile_letter all_tiles player] is true if [tile] is available in the hand of 
    [player]. Otherwise false *)
let valid_tile_in_hand tile_letter all_tiles player =
  let tiles_in_hand = location_tile all_tiles (Hand player) [] in
  char_in_collection tiles_in_hand tile_letter 

exception Invalid_Play

(** [update_tile_loc tile_letter all_tiles cell acc] updates [all_tiles] with 
    the tile in the bag with letter [tile_letter] to be in the board *)
let rec update_tile_loc tile_letter all_tiles cell acc =
  match all_tiles with
  | [] -> acc
  | h::t -> begin
      if h.letter = tile_letter && h.location = Bag
      then  
        acc@({id=h.id;
              letter=h.letter;
              point=h.point;
              location=Board cell;}::t)
      else update_tile_loc tile_letter t cell (h::acc)
    end

(** [tile_to_update tiles_in_bag tile_letter] is the first tile in 
    [tiles_in_bag] that has the letter [tile_letter]. 
    Requires: [tiles_in_bag] contains a tile with letter [tile_letter] *)
let rec tile_to_update tiles_in_bag tile_letter =
  match tiles_in_bag with
  | [] -> failwith "impossible"
  | h::t -> if h.letter = tile_letter then h else tile_to_update t tile_letter

(** [update_board_cells board_cells tile cell acc] updates contents of [cell]
    in [board_cells] to contain [tile]*)
let rec update_board_cells board_cells tile cell acc =
  match board_cells with
  | [] -> acc
  | (g,c)::t -> 
    if g = cell then 
      begin
        let new_tile = 
          {id=tile.id; letter=tile.letter; point=tile.point; location=Board cell} in
        acc@((cell, Some new_tile)::t)
      end
    else update_board_cells t tile cell ((g,c)::acc)

(** [play cell tile_letter state] if the state when tile with [tile_letter] is 
    put in [cell] given current state [state] *)
let play cell tile_letter state player = 
  if (not (valid_cell cell state.board && valid_tile tile_letter state.all_tiles)) 
  && valid_tile_in_hand tile_letter state.all_tiles player
  then raise Invalid_Play
  else begin
    let tiles_in_bag = location_tile all_tiles Bag [] in 
    let updated_tile = tile_to_update tiles_in_bag tile_letter in
    let updated_board = 
      {
        cells= update_board_cells state.board.cells updated_tile cell [];
        point_bonus=state.board.point_bonus;
      } in
    let updated_tiles = update_tile_loc tile_letter state.all_tiles cell [] in
    {
      all_tiles = updated_tiles;
      board = updated_board;
      players = state.players;
    }
  end

let init_state = {
  all_tiles = init_tiles_player2;
  board = init_board;
  players = [Player1 init_player1;Player2 init_player2];
}

(** [print_board board_cells acc x y] prints [board_cells] *)
let rec print_board board_cells acc x y = 
  if y = 0 && x = 0 then print_string("  0  1  2  3  4  5  6  7  8  9");
  if acc mod 10 = 0 then print_endline("");
  if x = 0 && y <> 10 then print_string(Int.to_string y);
  match board_cells with
  | [] -> print_string " ";
  | (grid,contents)::t -> 
    begin
      match contents with
      | Some tile -> 
        begin
          print_string ("[" ^ (Char.escaped (tile.letter)) ^ "]");
          if x = 9 then print_board t (acc + 1) 0 (y + 1)
          else print_board t (acc + 1) (x + 1) y
        end
      | None ->
        begin
          print_string "[ ]";
          if x = 9 then print_board t (acc + 1) 0 (y + 1)
          else print_board t (acc + 1) (x + 1) y
        end
    end

(** [print_board ()] prints the initial board cells *)
let print_init_board () = 
  let board_obj = init_state.board.cells in
  print_board board_obj 0 0 0

(** [print_board board] prints the board cells of [board] *)
let print_board board = 
  let board_obj = List.sort compare board.cells in
  print_board board_obj 0 0 0

let get_init_state () = 
  init_state

let rec string_of_tiles hand_tiles =
  match hand_tiles with 
  | [] -> ""
  | h::t -> (Char.escaped h.letter) ^ " " ^ (string_of_tiles t)

let print_hand player game_state =
  let hand_tiles = location_tile game_state.all_tiles (Hand player) [] in
  print_string ("Hand:  [ " ^ (string_of_tiles hand_tiles) ^ "]")