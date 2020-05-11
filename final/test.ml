open OUnit2
open Scrabble
open Command
open State

(* TEST PLAN: *)

(* 
Most of our tests are testing the small helper functions that are 
manipulating information about tile locations and our hand. This includes 
testing for correct movement between Hand, Board, and Bag of tiles.  

In testing what the whole Board contains, since there were too many tiles in 
the Board to test with OUnit tests, we tested via “make play”. Because our game 
displays based on results from the functions we implemented, this made it easier 
to see if tiles were correctly moving between the bag/board/hand. This was the 
same situation with testing our Bag of tiles. There’s more than 100 tiles in 
the Bag, so it’d be too hard to assert if the Bag consisted of certain things. 
Thus, we created all of these black box test cases that were developed for our 
Scrabble.ml functions. These functions were created to carry out 
straightforward results, so it was easy to see if they were doing what they 
needed to do just by asserting what the end result was supposed to be. 
*)

(******************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ******************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ] 
let init_state = Scrabble.get_init_state () 
let init_tiles = init_state.all_tiles
let init_board = init_state.board 

let location1 = Scrabble.Hand (Player1 {score = 0})
let location2 = Scrabble.Hand (Player2 {score = 0})

let end_state1 = play (0,1) 'A' init_state (Player1 {score=0})
let end_state2 = play (0,2) 'I' end_state1 (Player1 {score=0})
let end_state3 = play (1,0) 'A' init_state (Player1 {score=0})

let collection1 = 
  [{id=1; letter='A'; point=2; location=Bag}; 
   {id=2; letter='B'; point=3; location=Bag}]
let collection2 = []
let collection3 = 
  [{id=1; letter='A'; point=2; location= Hand (Player1 {score=0})};
   {id=2; letter='B'; point=3; location= Hand (Player1 {score=0})}]
let collection4 = 
  [{id=1; letter='A'; point=2; location= Board (2,2)}; 
   {id=2; letter='B'; point=3; location= Bag}]
let collection5 = 
  [{id=1; letter='A'; point=2; location= Bag}; 
   {id=2; letter='B'; point=3; location= Board (2,2)}]
let collection6 = 
  [{id=1; letter='A'; point=2; location= Board (3,3)}; 
   {id=2; letter='B'; point=3; location= Hand (Player1 {score=0})}]
let collection7 = 
  [{id=1; letter='A'; point=2; location= Board (3,3)}; 
   {id=2; letter='B'; point=3; location= Board (4,4)}]
let collection8 = 
  [{id=1; letter='A'; point=2; location= Hand (Player1 {score=0})}; 
   {id=2; letter='B'; point=3; location= Hand (Player1 {score=0})}; 
   {id=3; letter='C'; point=2; location= Hand (Player1 {score=0})}]
let collection9 = 
  [{id = 1; letter = 'A'; point = 2; location = Hand (Player1 {score = 0})};
   {id = 2; letter = 'B'; point = 3; location = Board (4, 4)}]
let collection10 = 
  [{id = 3; letter = 'C'; point = 2; location = Hand (Player1 {score = 0})};
   {id = 2; letter = 'B'; point = 3; location = Hand (Player1 {score = 0})};
   {id = 1; letter = 'A'; point = 2; location = Hand (Player1 {score = 0})}]
let collection11 = 
  [{id = 3; letter = 'C'; point = 2; location = Hand (Player1 {score = 0})};
   {id = 2; letter = 'B'; point = 3; location = Hand (Player2 {score = 0})};
   {id = 1; letter = 'A'; point = 2; location = Hand (Player1 {score = 0})}]

let scrabble_tests =
  [
    "valid_cell1" >:: 
    (fun _ -> assert_equal (valid_cell(1,1) init_board) true);
    "valid_cell2" >:: 
    (fun _ -> assert_equal (valid_cell(2,1) init_board) true);
    "valid_cell3" >:: 
    (fun _ -> assert_equal (valid_cell(9,10) init_board) false);

    "occupied_cell1" >:: 
    (fun _ -> assert_equal (occupied_cell (0,1) init_board) false);
    "occupied_cell2" >:: 
    (fun _ -> assert_equal (occupied_cell (1,1) init_board) false);
    "occupied_cell3" >::
    (fun _ -> assert_equal (occupied_cell (2,1) init_board) false);
    "occupied_cell4" >:: 
    (fun _ -> assert_equal (occupied_cell (0,0) init_board) true);

    "char_in_collection1" >:: 
    (fun _ -> assert_equal (char_in_collection collection1 'A') true);
    "char_in_collection2" >:: 
    (fun _ -> assert_equal (char_in_collection collection1 'B') true);
    "char_in_collection3" >:: 
    (fun _ -> assert_equal (char_in_collection collection1 'C') false);
    "char_in_collection4" >:: 
    (fun _ -> assert_equal (char_in_collection collection1 'D') false);
    "char_in_collection5" >:: 
    (fun _ -> assert_equal (char_in_collection collection2 'D') false);
    "char_in_collection6" >:: 
    (fun _ -> assert_equal (char_in_collection collection2 'A') false);

    "valid_tile1" >:: 
    (fun _ -> assert_equal (valid_tile 'A' collection1) true);
    "valid_tile2" >:: 
    (fun _ -> assert_equal (valid_tile 'B' collection1) true);
    "valid_tile3" >:: 
    (fun _ -> assert_equal (valid_tile 'C' collection1) false);

    "valid_tile_in_hand1" >:: 
    (fun _ -> assert_equal (valid_tile_in_hand 'A' collection3 
                              (Player1 {score=0})) true);
    "valid_tile_in_hand2" >:: 
    (fun _ -> assert_equal (valid_tile_in_hand 'B' collection3 
                              (Player1 {score=0})) true);
    "valid_tile_in_hand3" >:: 
    (fun _ -> assert_equal (valid_tile_in_hand 'C' collection3 
                              (Player1 {score=0})) false);

    "update_tile_loc1" >:: 
    (fun _ -> assert_equal (update_tile_loc 'A' collection1 (2,2) []) 
        collection4);
    "update_tile_loc3" >:: 
    (fun _ -> assert_equal (update_tile_loc 'B' collection2 (3,3) []) []);

    "tile_hand_to_board1" >:: 
    (fun _ -> assert_equal (tile_hand_to_board 'A' collection2 (1,1) [] 
                              (Player1 {score=0})) []);
    "tile_hand_to_board2" >:: 
    (fun _ -> assert_equal (tile_hand_to_board 'A' collection3 (3,3) [] 
                              (Player1 {score=0})) collection6);
    "tile_hand_to_board3" >:: 
    (fun _ -> assert_equal (tile_hand_to_board 'B' collection3 (4,4) [] 
                              (Player1 {score=0})) collection9);

    "tile_to_update1" >:: 
    (fun _ -> assert_equal (tile_to_update collection1 'A') 
        {id=1; letter= 'A'; point = 2; location= Bag});
    "tile_to_update2" >:: 
    (fun _ -> assert_equal (tile_to_update collection1 'B') 
        {id=2; letter= 'B'; point = 3; location= Bag});
    "tile_to_update3" >:: 
    (fun _ -> assert_equal (tile_to_update collection4 'B') 
        {id=2; letter= 'B'; point = 3; location= Bag});   
    "tile_to_update4" >:: 
    (fun _ -> assert_equal (tile_to_update collection5 'A') 
        {id=1; letter= 'A'; point = 2; location= Bag});   

    "string_of_tiles1" >:: 
    (fun _ -> assert_equal (string_of_tiles collection1) "A B ");
    "string_of_tiles2" >:: 
    (fun _ -> assert_equal (string_of_tiles collection2) "");
    "string_of_tiles3" >:: 
    (fun _ -> assert_equal (string_of_tiles collection3) "A B ");
    "string_of_tiles4" >:: 
    (fun _ -> assert_equal (string_of_tiles collection4) "A B ");
    "string_of_tiles5" >:: 
    (fun _ -> assert_equal (string_of_tiles collection5) "A B ");
    "string_of_tiles6" >:: 
    (fun _ -> assert_equal (string_of_tiles collection6) "A B ");
    "string_of_tiles7" >:: 
    (fun _ -> assert_equal (string_of_tiles collection8) "A B C ");

    "list_diff1" >:: 
    (fun _ -> assert_equal (list_diff collection2 collection1 []) 
        (List.rev collection1));
    "list_diff2" >:: 
    (fun _ -> assert_equal (list_diff collection1 collection2 []) []);
    "list_diff3" >:: 
    (fun _ -> assert_equal (list_diff collection2 collection8 []) 
        (List.rev collection8));
    "list_diff4" >:: 
    (fun _ -> assert_equal (list_diff collection1 collection8 []) collection10);

    "loc_in_tiles1" >:: 
    (fun _ -> assert_equal (loc_in_tiles collection2 (0,3)) false);
    "loc_in_tiles2" >:: 
    (fun _ -> assert_equal (loc_in_tiles collection4 (2,2)) true);
    "loc_in_tiles3" >:: 
    (fun _ -> assert_equal (loc_in_tiles collection4 (3,2)) false);

    "tiles_in_player_hand1" >:: 
    (fun _ -> assert_equal (List.length (tiles_in_player_hand collection3 
                                           "player1" [])) 2);
    "tiles_in_player_hand2" >:: 
    (fun _ -> assert_equal (List.length (tiles_in_player_hand collection3 
                                           "player2" [])) 0);
    "tiles_in_player_hand3" >:: 
    (fun _ -> assert_equal (List.length (tiles_in_player_hand collection11 
                                           "player1" [])) 2);
    "tiles_in_player_hand4" >:: 
    (fun _ -> assert_equal (List.length (tiles_in_player_hand collection11 
                                           "player2" [])) 1);
    "tiles_in_player_hand5" >:: 
    (fun _ -> assert_equal (List.length (tiles_in_player_hand collection1 
                                           "player1" [])) 0);

    "points_of_turn1" >:: 
    (fun _ -> assert_equal (points_of_turn init_state end_state1) 2);
    "points_of_turn2" >:: 
    (fun _ -> assert_equal (points_of_turn init_state end_state2) 3);
    "points_of_turn3" >:: 
    (fun _ -> assert_equal (points_of_turn init_state end_state3) 2);
  ]

let suite =
  "test suite for final"  >::: List.flatten [
    scrabble_tests;
  ]

let _ = run_test_tt_main suite
