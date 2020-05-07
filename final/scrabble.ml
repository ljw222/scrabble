exception Invalid_Play
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

(** [create_init_tiles tiles_points acc counter] creating initial tiles for 
    game *)
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

(** [occupied_cell cell board] is true if the [cell] contains a tile *)
let occupied_cell cell board = 
  let valid_grid_params = List.mem_assoc cell board.cells in 
  if not valid_grid_params then false
  else 
    let occup_grid = List.assoc cell board.cells in 
    match occup_grid with
    | Some t -> true
    | None -> false

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

let init_state = {
  all_tiles = init_tiles_player2;
  board = init_board;
  players = [Player1 init_player1;Player2 init_player2];
}

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

(** [tile_hand_to_board tile_letter all_tiles cell acc player] updates 
    [all_tiles] with the tile in the hand of [player] with letter [tile_letter] 
    to be in the board *)
let rec tile_hand_to_board tile_letter all_tiles cell acc player =
  match all_tiles with
  | [] -> acc
  | h::t -> begin
      if h.letter = tile_letter && h.location = (Hand player)
      then  
        acc@({id=h.id;
              letter=h.letter;
              point=h.point;
              location=Board cell;}::t)
      else tile_hand_to_board tile_letter t cell (h::acc) player
    end

(** [tile_board_to_hand all_tiles cell acc player] updates 
    [all_tiles] with the tile on the board with letter [tile_letter] 
    to be in the hand of [player] *)
let rec tile_board_to_hand all_tiles cell acc player =
  match all_tiles with
  | [] -> acc
  | h::t -> begin
      if h.location = (Board cell)
      then  
        acc@({id=h.id;
              letter=h.letter;
              point=h.point;
              location=Hand player;}::t)
      else tile_board_to_hand t cell (h::acc) player
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

(** [rem_board_cells board_cells tile cell acc] updates contents of [cell]
    in [board_cells] to None in the content*)
let rec rem_board_cells board_cells cell acc =
  match board_cells with
  | [] -> acc
  | (g,c)::t -> 
    if g = cell then acc@((cell, None)::t)
    else rem_board_cells t cell ((g,c)::acc)

(** [add_tile_to_board tiles_in_hand tile_letter cell] adds [tile_letter] from
    hand to [cell] on board.
    Requires: [tiles_in_hand] contains a tile with letter [tile_letter] *)
let rec add_tile_to_board tiles_in_hand tile_letter cell =
  match tiles_in_hand with
  | [] -> failwith "impossible"
  | h::t -> 
    if h.letter = tile_letter then 
      {id=h.id; letter=h.letter; point=h.point; location=Board cell}
    else add_tile_to_board t tile_letter cell

(** [return_tile_to_hand tiles_in_cell player] adds [tiles_in_cell] back to hand 
    of [player] from the board.*)
let rec return_tile_to_hand cell tiles_in_cell player =
  match tiles_in_cell with
  | [] -> failwith "impossible"
  | h::t -> 
    match h.location with
    | Board grid -> if grid = cell then 
        {id=h.id; letter=h.letter; point=h.point; location=Hand player}
      else return_tile_to_hand cell t player
    | _ -> failwith "not on board"

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

(** [get_init_state ()] the initial state of the game *)
let get_init_state () = 
  init_state

(** [string_of_tiles hand_tiles] string of list [hand_tiles] *)
let rec string_of_tiles hand_tiles =
  match hand_tiles with 
  | [] -> ""
  | h::t -> (Char.escaped h.letter) ^ " " ^ (string_of_tiles t)

(** [print_hand player game_state] prints the hand of [player] *)
let print_hand player game_state =
  let hand_tiles = location_tile game_state.all_tiles (Hand player) [] in
  print_string ("Hand:  [ " ^ (string_of_tiles hand_tiles) ^ "]")

(** [refill_hand state player] refills hand of [player] to 7 tiles after turn is
    a valid check *)
let refill_hand state player = 
  let tiles_in_hand = location_tile state.all_tiles (Hand player) [] in 
  let num_to_refill = 7 - (List.length tiles_in_hand) in 
  let updated_tiles = choose_tile state.all_tiles player num_to_refill in 
  {
    all_tiles = updated_tiles;
    board = state.board;
    players = state.players;
  }

(** [occupied_grids board_cells acc] is the list of tiles in [board_cells] *)
let rec occupied_grids board_cells acc =
  match board_cells with
  | [] -> List.rev acc
  | (g,c)::t -> 
    match c with
    | Some tile -> occupied_grids t (tile::acc)
    | None -> occupied_grids t acc

(** [list_diff l1 l2 acc] is the difference between [l1] and [l2] *)
let rec list_diff l1 l2 acc = 
  match l2 with
  | [] -> acc
  | h::t -> if List.mem h l1 then list_diff l1 t acc
    else list_diff l1 t (h::acc)

(** [same_row tile_grid_list x] is true if all tiles in [tile_grid_list] s
    hare the same [x] coordinate*)
let rec same_row (tile_list:tile list) x =
  match tile_list with
  | [] -> true
  | tile::t -> 
    begin 
      let g = match tile.location with 
        | Board grid -> grid
        | _ -> failwith "not on grid" in
      if (fst g) = x then true && (same_row t x) 
      else false
    end

(** [same_col tile_grid_list y] is true if all tiles in [tile_grid_list] s
    hare the same [y] coordinate*)
let rec same_col (tile_list:tile list) y =
  match tile_list with
  | [] -> true
  | tile::t -> 
    begin 
      let g = match tile.location with 
        | Board grid -> grid
        | _ -> failwith "not on grid" in
      if (snd g) = y then true && (same_col t y) 
      else false
    end

(** [cells_of_tiles tile_list acc] is the list of board locations of 
    [tile_list] *)
let rec cells_of_tiles tile_list acc =
  match tile_list with
  | [] -> List.rev acc
  | h::t -> 
    match h.location with
    | Board grid -> cells_of_tiles t (grid::acc)
    | _ -> cells_of_tiles t acc

(** [build_list start finish acc] is an numerical list from [start] to [finish] *)
let rec build_list start finish acc = 
  if finish = start then (finish::acc) else
    build_list start (finish - 1) (finish::acc)

(** [col_check_if_gaps_filled beg_cells x_lst y ordered_x] is true if there 
    is no gap in [ordered x] *)
let rec row_check_if_gaps_filled (beg_cells:grid list) (x_lst: int list) (y:int) 
    (ordered_x:int list) =
  match ordered_x with
  | [] -> true
  | x::t -> 
    if List.mem x x_lst then true && (row_check_if_gaps_filled beg_cells x_lst y t)
    else 
    if List.mem (x,y) beg_cells then 
      true && (row_check_if_gaps_filled beg_cells x_lst y t) else false

(** [helper_row x_lst beg_cells y] is true if the new tiles played in [x_list]
    are valid moves in comparison to tiles in [beg_cells] list*)
let helper_row x_lst beg_cells y =
  let fst_x = List.nth x_lst 0 in
  let last_x = List.nth (List.rev x_lst) 0 in 
  (* list of xs with no gaps *)
  let ordered_x = build_list fst_x last_x [] in
  (* case where original tile is first/last in row/col formed *)
  if List.length x_lst = (last_x - fst_x + 1) then 
    if (List.mem (fst_x - 1,y) beg_cells) || (List.mem (last_x + 1,y) beg_cells)
    then true
    else false
    (* case where original tiles are dispursed inbetween new word formed *)
  else row_check_if_gaps_filled beg_cells x_lst y ordered_x

(** [col_check_if_gaps_filled beg_cells y_lst x ordered_y] is true if there 
    is no gap in [ordered y] *)
let rec col_check_if_gaps_filled (beg_cells:grid list) (y_lst: int list) (x:int) 
    (ordered_y:int list) =
  match ordered_y with
  | [] -> true
  | y::t -> 
    if List.mem y y_lst then true && (col_check_if_gaps_filled beg_cells y_lst x t)
    else 
    if List.mem (x,y) beg_cells then 
      true && (col_check_if_gaps_filled beg_cells y_lst x t) else false

(** [helper_col y_lst beg_cells x] is true if the new tiles played in [y_list]
    are valid moves in comparison to tiles in [beg_cells] list *)
let helper_col y_lst beg_cells x =
  let fst_y = List.nth y_lst 0 in
  let last_y = List.nth (List.rev y_lst) 0 in 
  (* list of xs with no gaps *)
  let ordered_y = build_list fst_y last_y [] in
  (* case where original tile is first/last in row/col formed *)
  if List.length y_lst = (last_y - fst_y + 1) then 
    if (List.mem (x,fst_y - 1) beg_cells) || (List.mem (x, last_y + 1) beg_cells)
    then true
    else false
    (* case where original tiles are dispursed inbetween new word formed *)
  else col_check_if_gaps_filled beg_cells y_lst x ordered_y


(* returns true if all tiles on board in new_state are valid in comparison to beg_state  *)
let check_if_valid beg_state end_state =
  let beg_board_tiles = occupied_grids beg_state.board.cells [] in 
  let end_board_tiles = occupied_grids end_state.board.cells [] in 
  let new_tiles = list_diff beg_board_tiles end_board_tiles [] in
  (* first x coordinate in list of new tiles *)
  let first_x =
    match (List.nth new_tiles 0).location with 
    | Board (x,y) -> x 
    | _ -> failwith "not on board" in 
  let first_y = 
    match (List.nth new_tiles 0).location with 
    | Board (x,y) -> y 
    | _ -> failwith "not on board" in 
  (* checking if all new tiles are in same row/column *)
  let valid_new_tiles = 
    (same_row new_tiles first_x) || (same_col new_tiles first_y) in
  (* check if existing tiles are touching new tiles *)
  if not valid_new_tiles then 
    begin
      print_endline("New tiles are not placed in the same row/col");
      false
    end
  else
    (* if in same row, all y coordinates, if same col, all x  *)
    let new_grids = cells_of_tiles new_tiles [] in
    (* let contingent = [] in *)
    if (same_row new_tiles first_x) then 
      let y_list = (List.sort compare (snd (List.split new_grids))) in 
      helper_col y_list (cells_of_tiles beg_board_tiles []) first_x
    else
      let x_list = (List.sort compare (fst (List.split new_grids))) in 
      helper_row x_list (cells_of_tiles beg_board_tiles []) first_y

(** [play cell tile_letter state] if the state when tile with [tile_letter] is 
    put in [cell] given current state [state] *)
let play cell tile_letter state player = 
  if (valid_cell cell state.board) && (valid_tile_in_hand tile_letter state.all_tiles player) then
    begin
      let tiles_in_hand = location_tile state.all_tiles (Hand player) [] in 
      let updated_tile = add_tile_to_board tiles_in_hand tile_letter cell in
      let updated_board = 
        {
          cells= update_board_cells state.board.cells updated_tile cell [];
          point_bonus=state.board.point_bonus;
        } in
      let updated_tiles = tile_hand_to_board tile_letter state.all_tiles cell
          [] player in
      {
        all_tiles = updated_tiles;
        board = updated_board;
        players = state.players;
      }
    end
  else (if (not (valid_cell cell state.board)) then 
          (print_endline("Cell is not available"); raise Invalid_Play)
        else (print_endline("Tile is not available in your hand"); 
              raise Invalid_Play))


let delete cell state player = 
  if (occupied_cell cell state.board) 
  then (
    (* let tile_at_grid = location_tile state.all_tiles (Board cell) [] in  *)
    (* let update_tile = return_tile_to_hand cell tile_at_grid player in  *)
    let update_board = 
      {
        cells = rem_board_cells state.board.cells cell [];
        point_bonus=state.board.point_bonus;
      } in
    let updated_tiles = tile_board_to_hand state.all_tiles cell [] player in 
    {
      all_tiles = updated_tiles;
      board = update_board;
      players = state.players;
    }
  )
  else (if (not (valid_cell cell state.board)) then 
          (print_endline("Cell is not available"); raise Invalid_Play)
        else (print_endline("Tile is not available in selected cell"); 
              raise Invalid_Play))