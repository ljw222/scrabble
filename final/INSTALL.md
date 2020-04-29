We will be demoing functions in utop

1. Open utop and type: #use "scrabble.ml";;
2. Show that the players are initially created to have 0 points
    - init_player1;;
3. Demo what one tile looks like/what attributes it contains through the 
starting tile on the board
  -  starting_tile;;
4. All tiles of the game
    - all_tiles;;
5. Once game is started, 7 tiles are dealt to hand of players
    - init_tiles_player1;;
    - init_tiles_player2;;
6. Initial board of game
    - init_board;;
7. Can't add multiple tiles in same grid location on board
    - play (6,6) 'B' init_state;;
8. Showing play function (adding tile to board)
    - play (5,5) 'B' init_state;;
9. Can only add tiles to the board that are in the bag
    - can't add invalid tile
          - play (2,2) '.' init_state;;
    - if tile exists but is not in the bag, can't add to board (there are 4 'B's to start out with in the board)
          - let empty_board = { all_tiles = all_tiles; board = init_board; players = [];};;
          Following commands should be VALID:
          - let b1 = play (0,1) 'B' empty_board;;
          - let b2 = play (0,2) 'B' b1;;
          - let b3 = play (0,3) 'B' b2;;
          - let b4 = play (0,4) 'B' b3;;
          This will be INVALID:
          - play (0,5) 'B' b4;;