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
  starting_tile::(create_init_tiles 
                    (char_tiles@char_tiles@char_tiles@char_tiles) [] 1)

(** [location_tile tiles location acc] is the list of tiles from [tiles] 
    that are located in [location]*)
let rec location_tile tiles location acc =
  match tiles with
  | [] -> List.rev acc
  | h::t -> 
    if h.location = location then location_tile t location (h::acc) else 
      location_tile t location acc

(** [choose_tile tiles player num_tiles] all tiles in [tiles] with a random tile 
    location changed to be in the hand of [player] [num_tiles] times *)
let rec choose_tile tiles player num_tiles =
  if num_tiles = 0 then tiles 
  else
    let bag_tiles = location_tile tiles Bag [] in
    let rand = Random.int((List.length bag_tiles) - 1) in
    let tile = List.nth tiles rand in
    let new_tile = 
      {id=tile.id; letter=tile.letter; point= tile.point; location=Hand player} 
    in
    let updated_tiles = List.map (fun old_tile -> 
        if old_tile.id = new_tile.id then new_tile else old_tile) tiles in
    choose_tile updated_tiles player (num_tiles - 1)

let init_tiles_player1 = 
  choose_tile all_tiles (Player1 init_player1) 12

let init_tiles_player2 = 
  choose_tile init_tiles_player1 (Player2 init_player1) 11

(** [create_init_board acc x y] creates the initial board state with dimensions 
    [x] x [y] *)
let rec create_init_board acc x y = 
  match x,y with
  | 0,0 -> acc
  | x,y ->
    begin
      if y = 0 then create_init_board (((x,y), None)::acc) (x-1) (9) else
        match x,y with
        | x,y -> 
          if (y = 0 && x != 0) then 
            create_init_board (((x,y), None)::acc) (x-1) (y)
          else if (y != 0 && x <= 9) then 
            create_init_board (((x,y), None)::acc) (x) (y-1)
          else create_init_board (((x,y), None)::acc) (x-1) (9)
    end

(** [create_init_bonus1 acc x y] creates bonus square diagonal 1 on board with 
    dimensions [x] x [y]*)
let rec create_init_bonus1 acc x y = 
  if x >= 0 && y >= 0 then create_init_bonus1 (((x,y),2)::acc) (x-1) (y-1)
  else acc

(** [create_init_bonus2 acc x y] creates bonus square diagonal 2 on board with 
    dimensions [x] x [y]*)
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

let valid_cell cell board = 
  let valid_grid_params = List.mem_assoc cell board.cells in 
  if not valid_grid_params then false
  else 
    let avail_grid = List.assoc cell board.cells in 
    avail_grid = None

let occupied_cell cell board = 
  let valid_grid_params = List.mem_assoc cell board.cells in 
  if not valid_grid_params then false
  else 
    let occup_grid = List.assoc cell board.cells in 
    match occup_grid with
    | Some t -> true
    | None -> false

(** [player_type player] player type of [player] *)
let player_type player= 
  match player with
  | Player1 _ -> "player1"
  | Player2 _ -> "player2" 

let rec tiles_in_player_hand tiles player_type acc =
  match tiles with
  | [] -> List.rev acc
  | h::t -> 
    begin
      let tile_loc = 
        match h.location with
        | Hand (Player1 _) -> "player1"
        | Hand (Player2 _) -> "player2"
        | _ -> "" in 
      if tile_loc = player_type then tiles_in_player_hand t player_type (h::acc) 
      else tiles_in_player_hand t player_type acc
    end

let rec char_in_collection collection char = 
  match collection with
  | [] -> false
  | h::t -> if h.letter = char then true else char_in_collection t char

let valid_tile tile_letter all_tiles =
  let bag_tiles = location_tile all_tiles Bag [] in
  char_in_collection bag_tiles tile_letter 

let valid_tile_in_hand tile_letter all_tiles player =
  let tiles_in_hand = tiles_in_player_hand all_tiles (player_type player) [] in
  char_in_collection tiles_in_hand tile_letter 

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

let rec tile_to_update tiles_in_bag tile_letter =
  match tiles_in_bag with
  | [] -> failwith "impossible"
  | h::t -> if h.letter = tile_letter then h else tile_to_update t tile_letter

let rec update_board_cells board_cells tile cell acc =
  match board_cells with
  | [] -> acc
  | (g,c)::t -> 
    if g = cell then 
      begin
        let new_tile = 
          {id=tile.id; letter=tile.letter; point=tile.point; 
           location=Board cell} in
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

let rec print_board board_cells acc x y = 
  if y = 0 && x = 0 then print_string("  0  1  2  3  4  5  6  7  8  9");
  if acc mod 10 = 0 then print_endline("");
  if x = 0 && y <> 10 then print_string(Int.to_string y);
  match board_cells with
  | [] -> print_string " "; print_endline("");
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

let print_init_board () = 
  let board_obj = init_state.board.cells in
  print_board board_obj 0 0 0

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
  let player_type = player_type player in 
  let hand_tiles = tiles_in_player_hand game_state.all_tiles player_type [] in
  begin
    print_string ("Hand:  [ " ^ (string_of_tiles hand_tiles) ^ "]");
    print_endline("");
  end

(**[update_player player_type score] updates player of type [player_type] to 
   have the new score [score] *)
let update_player player_type score = 
  if player_type = "player1" then (Player1 {score=score}) 
  else (Player2 {score=score})

let refill_hand state player score = 
  let player_type = player_type player in
  let tiles_in_hand = tiles_in_player_hand state.all_tiles player_type [] in
  let num_to_refill = 10 - (List.length tiles_in_hand) in 
  let updated_tiles = choose_tile state.all_tiles player num_to_refill in 
  if player_type = "player1" then (
    {
      all_tiles = updated_tiles;
      board = state.board;
      players = match state.players with
        | [] -> []
        | h::t -> (update_player "player1" score)::t
    }
  )
  else 
    {
      all_tiles = updated_tiles;
      board = state.board;
      players = match state.players with
        | [] -> []
        | h::t -> h::[update_player "player2" score]
    }

(** [occupied_grids board_cells acc] is the list of tiles in [board_cells] *)
let rec occupied_grids board_cells acc =
  match board_cells with
  | [] -> List.rev acc
  | (g,c)::t -> 
    match c with
    | Some tile -> occupied_grids t (tile::acc)
    | None -> occupied_grids t acc

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

(** [build_list start finish acc] is an numerical list from [start] to 
    [finish] *)
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
    if List.mem x x_lst then true && 
                             (row_check_if_gaps_filled beg_cells x_lst y t)
    else 
    if List.mem (x,y) beg_cells then 
      true && (row_check_if_gaps_filled beg_cells x_lst y t) else false

(** [helper_row x_lst beg_cells y] is true if the new tiles played in [x_list]
    are valid moves in comparison to tiles in [beg_cells] list *)
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
    if List.mem y y_lst then true && 
                             (col_check_if_gaps_filled beg_cells y_lst x t)
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
      ANSITerminal.(print_string [red]
                      "\nNew tiles are not placed in the same row/col\n");
      false
    end
  else
    begin
      let new_grids = cells_of_tiles new_tiles [] in
      if List.length new_tiles = 1 then 
        begin
          let y_list = (List.sort compare (snd (List.split new_grids))) in 
          let check_same_row = helper_col y_list (cells_of_tiles beg_board_tiles 
                                                    []) first_x in
          let x_list = (List.sort compare (fst (List.split new_grids))) in 
          let check_same_col = helper_row x_list (cells_of_tiles beg_board_tiles 
                                                    []) first_y in 
          check_same_row || check_same_col
        end 
      else
      if (same_row new_tiles first_x) then 
        let y_list = (List.sort compare (snd (List.split new_grids))) in 
        helper_col y_list (cells_of_tiles beg_board_tiles []) first_x
      else
        let x_list = (List.sort compare (fst (List.split new_grids))) in 
        helper_row x_list (cells_of_tiles beg_board_tiles []) first_y
    end

let rec loc_in_tiles tiles grid_loc = 
  match tiles with
  | [] -> false
  | h::t -> 
    match h.location with 
    | Board g -> if g = grid_loc then true else loc_in_tiles t grid_loc
    | _ -> loc_in_tiles t grid_loc

(** [tile_with_loc tiles grid_loc] it the tile in [tiles] list with location on
    the board of [grid_loc] *)
let rec tile_with_loc tiles grid_loc = 
  match tiles with
  | [] -> failwith "tile in grid_loc is not in tiles list"
  | h::t -> 
    match h.location with 
    | Board g -> if g = grid_loc then h else tile_with_loc t grid_loc
    | _ -> tile_with_loc t grid_loc

(** [tiles_in_row_above beg_board_tiles smallest_y_coord x_loc acc] is the list 
    of tiles that are in the column above/next location 
    [x_loc],[smallest_y_coord] *)
let rec tiles_in_row_above beg_board_tiles smallest_y_coord x_loc acc =
  let smallest_y_coord = smallest_y_coord - 1 in 
  if loc_in_tiles beg_board_tiles (x_loc, smallest_y_coord) then 
    begin
      let new_tile = tile_with_loc beg_board_tiles (x_loc, smallest_y_coord) in
      tiles_in_row_above beg_board_tiles smallest_y_coord x_loc (new_tile::acc)
    end
  else acc

(** [tiles_in_col_left beg_board_tiles smallest_x_coord y_loc acc] is the list 
    of tiles that are in the column to the left/next location 
    [y_loc],[smallest_x_coord] *)
let rec tiles_in_col_left beg_board_tiles smallest_x_coord y_loc acc =
  let smallest_x_coord = smallest_x_coord - 1 in 
  if loc_in_tiles beg_board_tiles (smallest_x_coord, y_loc) then 
    begin
      let new_tile = tile_with_loc beg_board_tiles (smallest_x_coord, y_loc) in
      tiles_in_col_left beg_board_tiles smallest_x_coord y_loc (new_tile::acc)
    end
  else acc

(** [tiles_in_row_below beg_board_tiles largest_y_coord x_loc acc] is the list 
    of tiles that are in the column below/next location 
    [x_loc],[smallest_y_coord]*)
let rec tiles_in_row_below beg_board_tiles largest_y_coord x_loc acc =
  let largest_y_coord = largest_y_coord + 1 in 
  if loc_in_tiles beg_board_tiles (x_loc, largest_y_coord) then 
    begin
      let new_tile = tile_with_loc beg_board_tiles (x_loc, largest_y_coord) in
      tiles_in_row_below beg_board_tiles largest_y_coord x_loc (new_tile::acc)
    end
  else List.rev acc

(** [tiles_in_col_right beg_board_tiles largest_x_coord y_loc acc] is the list 
    of tiles that are in the column to the right/next location 
    [y_loc],[smallest_x_coord]*)
let rec tiles_in_col_right beg_board_tiles largest_x_coord y_loc acc =
  let largest_x_coord = largest_x_coord + 1 in 
  if loc_in_tiles beg_board_tiles (largest_x_coord, y_loc) then 
    begin
      let new_tile = tile_with_loc beg_board_tiles (largest_x_coord, y_loc) in
      tiles_in_col_right beg_board_tiles largest_x_coord y_loc (new_tile::acc)
    end
  else List.rev acc

(** [tiles_in_col_middle beg_board_tiles y_list x_loc acc] is all the tiles in
    boead [beg_board_tiles] with y coordinate in [y_list] and x coordinate 
    [x_loc] *)
let rec tiles_in_col_middle beg_board_tiles smallest_y largest_y x_loc acc = 
  if smallest_y <= largest_y then
    begin
      let tile_in_board = 
        tile_with_loc beg_board_tiles (x_loc, smallest_y) in 
      tiles_in_col_middle beg_board_tiles (smallest_y + 1) 
        largest_y x_loc (tile_in_board::acc)
    end
  else List.rev acc

(** [tiles_in_row_middle beg_board_tiles x_list y_loc acc] is all the tiles in
    boead [beg_board_tiles] with x coordinate in [x_list] and y coordinate 
    [x_loc] *)
let rec tiles_in_row_middle beg_board_tiles smallest_x largest_x y_loc acc = 
  if smallest_x <= largest_x then
    begin
      let tile_in_board = 
        tile_with_loc beg_board_tiles (smallest_x, y_loc) in 
      tiles_in_row_middle beg_board_tiles (smallest_x + 1) 
        largest_x y_loc (tile_in_board::acc)
    end
  else List.rev acc

(** [entire_word_in_col beg_board_tiles y_list x_loc new_tiles] tiles of all 
    tiles in word in column *)
let entire_word_in_col beg_board_tiles y_list x_loc new_tiles =
  (* lowest y coordinate of tiles in a col *)
  let smallest_y_coord = List.nth y_list 0 in 
  (* highest y coordinate of tiles in a col *)
  let largest_y_coord = List.nth (List.rev y_list) 0 in 
  let tiles_above = 
    tiles_in_row_above beg_board_tiles smallest_y_coord x_loc [] in
  let tiles_below = 
    tiles_in_row_below beg_board_tiles largest_y_coord x_loc [] in
  let tiles_in_middle = 
    tiles_in_col_middle (List.append beg_board_tiles new_tiles) 
      smallest_y_coord largest_y_coord x_loc [] in
  let all_tiles_in_word = tiles_above@tiles_in_middle@tiles_below in
  all_tiles_in_word

(** [entire_word_in_col beg_board_tiles x_list y_loc new_tiles] tiles of all 
    tiles in word in column *)
let entire_word_in_row beg_board_tiles x_list y_loc new_tiles =
  (* lowest y coordinate of tiles in a col *)
  let smallest_x_coord = List.nth x_list 0 in 
  (* highest y coordinate of tiles in a col *)
  let largest_x_coord = List.nth (List.rev x_list) 0 in 
  let tiles_left = 
    tiles_in_col_left beg_board_tiles smallest_x_coord y_loc [] in
  let tiles_right = 
    tiles_in_col_right beg_board_tiles largest_x_coord y_loc [] in
  let tiles_in_middle = 
    tiles_in_row_middle (List.append beg_board_tiles new_tiles) 
      smallest_x_coord largest_x_coord y_loc [] in
  let all_tiles_in_word = tiles_left@tiles_in_middle@tiles_right in
  all_tiles_in_word

(** [in_same_row tile_grid_list x] is true if all tiles in [tile_grid_list] s
    hare the same [x] coordinate *)
let rec in_same_row (tile_list:tile list) y =
  match tile_list with
  | [] -> true
  | tile::t -> 
    begin 
      let g = match tile.location with 
        | Board grid -> grid
        | _ -> failwith "not on grid" in
      if (snd g) = y then true && (in_same_row t y) 
      else false
    end

(** [in_same_col tile_grid_list y] is true if all tiles in [tile_grid_list] s
    hare the same [y] coordinate *)
let rec in_same_col (tile_list:tile list) x =
  match tile_list with
  | [] -> true
  | tile::t -> 
    begin 
      let g = match tile.location with 
        | Board grid -> grid
        | _ -> failwith "not on grid" in
      if (fst g) = x then true && (in_same_col t x) 
      else false
    end

(** [find_horiz_word beg_board_tiles new_tile acc] the horizontal word in 
    [beg_board_tiles] to tile [new_tile] *)
let find_horiz_word beg_board_tiles new_tile acc =
  let y_coord = 
    match new_tile.location with
    | Board (x,y) -> y
    | _ -> failwith "tile not on the board" in 
  let x_coord = 
    match new_tile.location with
    | Board (x,y) -> x
    | _ -> failwith "tile not on the board" in 
  entire_word_in_col beg_board_tiles [y_coord] x_coord [new_tile]

(** [find_vert_word beg_board_tiles new_tile acc] the vertical word in 
    [beg_board_tiles] to tile [new_tile] *)
let find_vert_word beg_board_tiles new_tile acc =
  let y_coord = 
    match new_tile.location with
    | Board (x,y) -> y
    | _ -> failwith "tile not on the board" in 
  let x_coord = 
    match new_tile.location with
    | Board (x,y) -> x
    | _ -> failwith "tile not on the board" in 
  entire_word_in_row beg_board_tiles [x_coord] y_coord [new_tile]

(** [horizontal_secondary_words beg_board_tiles new_tiles acc] is the 
    secondary words (which are in horizontal) to the new tiles placed when
    they are vertically placed *)
let rec horizontal_secondary_words beg_board_tiles new_tiles acc = 
  match new_tiles with 
  | [] -> acc
  | new_tile::t -> 
    begin
      let word = find_horiz_word beg_board_tiles new_tile [] in
      let new_words = horizontal_secondary_words beg_board_tiles t (word::acc) 
      in 
      List.filter (fun x -> if List.length x <> 1 then true else false) 
        new_words
    end

(** [vertical_secondary_words beg_board_tiles new_tiles acc] is the 
    secondary words (which are in horizontal) to the new tiles placed when
    they are vertically placed *)
let rec vertical_secondary_words beg_board_tiles new_tiles acc = 
  match new_tiles with 
  | [] -> acc
  | new_tile::t -> 
    begin
      let word = find_vert_word beg_board_tiles new_tile [] in
      let new_words = vertical_secondary_words beg_board_tiles t (word::acc) in 
      List.filter (fun x -> if List.length x <> 1 then true else false) 
        new_words
    end

(** [new_words_len1 new_grids beg_board_tiles first_x first_y new_tiles] is a 
    helper function for new_words that is able to find all new_words made when 
    only one tile is placed on the board *)
let new_words_len1 new_grids beg_board_tiles first_x first_y new_tiles = 
  let y_list = (List.sort compare (snd (List.split new_grids))) in
  let y_words = 
    entire_word_in_col beg_board_tiles y_list first_x new_tiles in 
  let x_list = (List.sort compare (fst (List.split new_grids))) in 
  let x_words = 
    entire_word_in_row beg_board_tiles x_list first_y new_tiles in 
  if (List.length x_words) > (List.length y_words) then
    begin
      let primary_word = x_words in 
      let secondary_words = horizontal_secondary_words beg_board_tiles new_tiles [] in 
      primary_word::secondary_words
    end
  else
    begin
      let primary_word = y_words in 
      let secondary_words = vertical_secondary_words beg_board_tiles new_tiles [] in 
      primary_word::secondary_words
    end

(** [new_words_mult new_tiles first_y first_x new_grids beg_board_tiles] is a 
    helper function for new_words that is able to find all new_words made when 
    more than one tile is placed on the board *)
let new_words_mult new_tiles first_y first_x new_grids beg_board_tiles = 
  if (in_same_row new_tiles first_y) then 
    begin
      let x_list = (List.sort compare (fst (List.split new_grids)))  in 
      let primary_word = 
        entire_word_in_row beg_board_tiles x_list first_y new_tiles in 
      let secondary_words = 
        horizontal_secondary_words beg_board_tiles new_tiles [] in 
      primary_word::secondary_words
    end
  else   
    begin
      let y_list = (List.sort compare (snd (List.split new_grids))) in
      let primary_word = 
        entire_word_in_col beg_board_tiles y_list first_x new_tiles in
      let secondary_words = 
        vertical_secondary_words beg_board_tiles new_tiles [] in 
      primary_word::secondary_words
    end 

(** [new_words beg_state end_state] are all of the new words (in tile list form) 
    from tiles that are placed in [end_state] from [beg_state] *)
let new_words beg_state end_state =
  let beg_board_tiles = occupied_grids beg_state.board.cells [] in 
  let end_board_tiles = occupied_grids end_state.board.cells [] in 
  let new_tiles = List.rev (list_diff beg_board_tiles end_board_tiles []) in
  let first_x =
    match (List.nth new_tiles 0).location with 
    | Board (x,y) -> x 
    | _ -> failwith "not on board" in 
  let first_y = 
    match (List.nth new_tiles 0).location with 
    | Board (x,y) -> y 
    | _ -> failwith "not on board" in 
  let new_grids = cells_of_tiles new_tiles [] in
  if List.length new_tiles = 1 then 
    new_words_len1 new_grids beg_board_tiles first_x first_y new_tiles
  else 
    new_words_mult new_tiles first_y first_x new_grids beg_board_tiles

(** [word_tiles_string tile_list acc] the word formed by tiles in [tile_list] *)
let rec word_tiles_string tile_list acc = 
  match tile_list with 
  | [] -> acc
  | tile::t -> word_tiles_string t (acc ^ (Char.escaped tile.letter))

(** [list_list_tiles_string tile_list_list acc] string of all words formed in 
    the list of list of tiles [tile_list_list] *)
let rec lst_of_tile_lst tile_list_list acc = 
  match tile_list_list with 
  | [] -> String.sub acc 2 ((String.length acc) - 2)
  | tile_lst::t ->
    lst_of_tile_lst t (", " ^ (word_tiles_string tile_lst "") ^ acc)

let print_words beg_state end_state = 
  let all_new_words = new_words beg_state end_state in 
  print_endline(lst_of_tile_lst all_new_words "")

(** [location_lst new_tiles acc] is the location_id list of tiles in 
    [new_tiles] *)
let rec location_lst new_tiles acc = 
  match new_tiles with
  | h::t-> location_lst t (h.location::acc)
  | [] -> acc

(** [grid_lst loc_lst acc] is the grid list of tiles with locations in  
    [loc_lst] *)
let rec grid_lst loc_lst acc = 
  match loc_lst with
  | (Board grid )::t -> grid_lst t (grid::acc)
  | _ :: t-> grid_lst t acc
  | [] -> acc

let delete cell state player beg_state end_state = 
  let beg_board_tiles = occupied_grids beg_state.board.cells [] in 
  let end_board_tiles = occupied_grids end_state.board.cells [] in 
  let new_tiles = list_diff beg_board_tiles end_board_tiles [] in
  let new_loc = location_lst new_tiles [] in
  let new_grid = grid_lst new_loc [] in

  if (occupied_cell cell state.board) && (List.mem cell new_grid) then 
    begin
      let update_board = 
        { cells = rem_board_cells state.board.cells cell [];
          point_bonus=state.board.point_bonus; } in
      let updated_tiles = tile_board_to_hand state.all_tiles cell [] player in 
      { all_tiles = updated_tiles; board = update_board;
        players = state.players; }
    end
  else if (not (valid_cell cell state.board)) then 
    (print_endline("Cell is not available"); raise Invalid_Play)
  else 
    (print_endline("Tile is not available in selected cell");raise Invalid_Play)

let play cell tile_letter state player = 
  if (valid_cell cell state.board) && 
     (valid_tile_in_hand tile_letter state.all_tiles player) then
    begin
      let tiles_in_hand = 
        tiles_in_player_hand state.all_tiles (player_type player) [] in
      let updated_tile = add_tile_to_board tiles_in_hand tile_letter cell in
      let updated_board = 
        { cells= update_board_cells state.board.cells updated_tile cell [];
          point_bonus=state.board.point_bonus; } in
      let updated_tiles = tile_hand_to_board tile_letter state.all_tiles cell
          [] player in
      { all_tiles = updated_tiles; board = updated_board;
        players = state.players; }
    end
  else if (not (valid_cell cell state.board)) then 
    (print_endline("Cell is not available"); raise Invalid_Play)
  else 
    (print_endline("Tile is not available in your hand"); raise Invalid_Play)

let return_current_score player = 
  match player with
  | Player1 p1 -> p1.score
  | Player2 p2 -> p2.score

let return_current_score2 player_type state = 
  if player_type = "player1" then 
    match state.players with
    | [] -> 0
    | h::t -> 
      match h with
      | Player1 p1 -> p1.score
      | Player2 p2 -> p2.score
  else
    match List.rev state.players with
    | [] -> 0
    | h::t -> 
      match h with
      | Player1 p1 -> p1.score
      | Player2 p2 -> p2.score

(** [points_of_word tile_list acc] the points of word tiles in [tile_list] *)
let rec points_of_word tile_list acc = 
  match tile_list with 
  | [] -> acc
  | tile::t -> points_of_word t (tile.point + acc)

(** [list_of_word_tiles tile_list_list acc] points of all words formed in 
    the list of list of tiles [lst_of_tile_lst] *)
let rec list_of_word_tiles lst_of_tile_lst acc = 
  match lst_of_tile_lst with 
  | [] -> acc
  | tile_lst::t ->
    list_of_word_tiles t ((points_of_word tile_lst 0) + acc)

let points_of_turn beg_state end_state = 
  let all_new_words = new_words beg_state end_state in 
  list_of_word_tiles all_new_words 0

let rec update_player1 new_score current_state acc = 
  match current_state.all_tiles with
  | h::t ->
    begin
      match h.location with
      | Hand player -> if ((player_type player)="player1") then 
          (update_player1 new_score current_state 
             ({
               id= h.id;
               letter = h.letter; 
               point= h.point;
               location = Hand (Player1 {score = new_score});
             }::acc)) 
        else (update_player1 new_score current_state acc)
      | _ -> update_player1 new_score current_state acc
    end
  | [] -> print_endline("not in player1");
    failwith "not in player1"

(** [update_player2 new_score current_state acc] updates player2's score in 
    [current_state] to be [new_score] *)
let rec update_player2 new_score current_state acc = 
  match current_state.all_tiles with
  | h::t ->
    begin
      match h.location with
      | Hand player -> if ((player_type player)="player2") then 
          (update_player1 new_score current_state 
             ({
               id= h.id;
               letter = h.letter; 
               point= h.point;
               location = Hand (Player1 {score = new_score});
             }::acc)) 
        else (update_player2 new_score current_state acc)
      | _ -> update_player2 new_score current_state acc
    end
  | [] -> print_endline("not in player1");
    failwith "not in player1"

let update_state new_score current_state player = 
  if (player_type player) = "player1" then 
    {
      all_tiles= (update_player1 new_score current_state []);
      board=current_state.board;
      players=current_state.players
    }
  else if ((player_type player) = "player2") then 
    {
      all_tiles= (update_player2 new_score current_state []);
      board=current_state.board;
      players=current_state.players
    }
  else failwith "not in either player's hand"