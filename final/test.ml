open OUnit2
open Scrabble
open Command
open State



(* Test Plan *)
(*  Most of our tests are testing the small*)






(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

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


let collection1 = [ {id=1; letter='A'; point=2; location=Bag} 
                  ; {id=2; letter='B'; point=3; location=Bag} ]
let collection2 = []
let collection3 = [ {id=1; letter='A'; point=2; location= Hand (Player1 {score=0})} 
                  ; {id=2; letter='B'; point=3; location= Hand (Player1 {score=0})} ]
let collection4 = [ {id=1; letter='A'; point=2; location= Board (2,2)} 
                  ; {id=2; letter='B'; point=3; location= Bag} ]
let collection5 = [ {id=1; letter='A'; point=2; location= Bag} 
                  ; {id=2; letter='B'; point=3; location= Board (2,2)} ]

let collection6 = [ {id=1; letter='A'; point=2; location= Board (3,3)} 
                  ; {id=2; letter='B'; point=3; location= Hand (Player1 {score=0})} ]

let collection7 = [ {id=1; letter='A'; point=2; location= Board (3,3)} 
                  ; {id=2; letter='B'; point=3; location= Board (4,4)} ]
let collection8 = [ {id=1; letter='A'; point=2; location= Hand (Player1 {score=0})} 
                  ; {id=2; letter='B'; point=3; location= Hand (Player1 {score=0})} 
                  ; {id=2; letter='C'; point=2; location= Hand (Player1 {score=0})} ]

(* let end_state1 = play (0,1) 'A' init_state (Player1 init_player1) *)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)

let scrabble_tests =

  assert_equal (valid_cell(1,1) init_board) true;
  assert_equal (valid_cell(2,1) init_board) true;
  assert_equal (valid_cell(9,9) init_board) true;

  (* NEED TO PUT THIS FUNC INTO THE SCRABBLE MLI  *)
  (* assert_equal (occupied_cell (0,1) init_board) true;
     assert_equal (occupied_cell (1,1) init_board) false;
     assert_equal (occupied_cell (2,1) init_board) true;
     assert_equal (occupied_cell (9,9) init_board) false; *)

  assert_equal (char_in_collection collection1 'A') true;
  assert_equal (char_in_collection collection1 'B') true;
  assert_equal (char_in_collection collection1 'C') false;
  assert_equal (char_in_collection collection1 'D') false;
  assert_equal (char_in_collection collection2 'D') false;
  assert_equal (char_in_collection collection2 'A') false;

  assert_equal (valid_tile 'A' collection1 ) true;
  assert_equal (valid_tile 'B' collection1 ) true;
  assert_equal (valid_tile 'C' collection1 ) false;

  (* NEED TO PUT THIS FUNC INTO THE SCRABBLE MLI  *)
  (* assert_equal (valid_tile_in_hand 'A' collection3(Player1 {score=0})) true;
     assert_equal (valid_tile_in_hand 'B' collection3(Player1 {score=0})) true;
     assert_equal (valid_tile_in_hand 'C' collection3(Player1 {score=0})) false; *)

  (* NEED TO PUT THIS FUNC INTO THE SCRABBLE MLI  *)
  (* assert_equal (update_tile_in_loc 'A' collection1 (2,2) []) collection4;
     assert_equal (update_tile_in_loc 'B' collection1 (3,3) []) collection5;
     assert_equal (update_tile_in_loc 'B' collection2 (3,3) []) []; *)


  (* assert_equal (tile_hand_to_board ‘A’ collection2 (1,1) [] (Player1 {score=0}) ) []
     assert_equal (tile_hand_to_board ‘A’ collection3 (3,3) [] (Player1 {score=0}) ) collection6
     assert_equal (tile_hand_to_board ‘B’ collection3 (4,4) [] (Player1 {score=0}) ) collection7 *)

  (* assert_equal (tile_hand_to_board 'A' collection2 (1,1) [] (Player1 {score=0}) ) [];
     assert_equal (tile_hand_to_board 'A' collection6 (3,3) [] (Player1 {score=0}) ) collection3;
     assert_equal (tile_board_to_hand 'B' collection7 (4,4) [] (Player1 {score=0}) ) collection3; *)
  (* 
assert_equals (tile_to_update collection1 ‘A’) {id=1; letter= ‘A’; point = 2; location= Bag };
assert_equals (tile_to_update collection1 ‘B’) {id=2; letter= ‘B’; point = 3; location= Bag };
assert_equals (tile_to_update collection4 ‘B’) {id=2; letter= ‘B’; point = 3; location= Bag};
assert_equals (tile_to_update collection5 ‘A’)  {id=1; letter= ‘A’; point = 2; location= Bag }; 

assert_equals (string_of_tiles collection1) “A B”;
assert_equals (string_of_tiles collection2) “”;
assert_equals (string_of_tiles collection3) “A B”;
assert_equals (string_of_tiles collection4) “A B”;
assert_equals (string_of_tiles collection5) “A B”;
assert_equals (string_of_tiles collection6) “A B”;
assert_equals (string_of_tiles collection8) “A B C”;


assert_equals ( list_diff collection2 collection1 []) collection2;
assert_equals ( list_diff collection1 collection2 []) [];
assert_equals ( list_diff collection2 collection8 []) collection8;
assert_equals ( list_diff collection1 collection8 []) [{id=3; letter= ‘C’; point = 2; location=  Hand Player1 {score=0} }];

assert_equals ( loc_in_tiles collection2 (0,3) ) false;
assert_equals ( loc_in_tiles collection4 (2,2) ) true;
assert_equals ( loc_in_tiles collection4 (3,2) ) false;


assert_equals (print_hand (Player1 {score=0}) init_state) “[ P N L A O T I ]”;
assert_equals (print_hand (Player1 {score=0}) end_state1) “[ L N P O T I ]”;
assert_equals (print_hand (Player1 {score=0}) end_state2) “[ T O P N L ]”; *)


  (* let command_tests =


     let state_tests =
  *)

  let suite =
    (* "test suite for A2"  >::: List.flatten [ *)
    scrabble_tests
(* command_tests;
   state_tests; *)
(* ] *)

let _ = run_test_tt_main suite 

