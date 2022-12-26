open OUnit2
open Chess
open Board

open General
(** Approach to testing: the approach to testing was to achieve acceptable
    Bisect coverage on all functions related to core game functionality,
    prioritizing testing on things like game logic over user interface
    generation. Once acceptable coverage was achieved, test cases for multiple
    possibilities for each circumstance were implemented (for example, testing
    possible moves on not just a pawn, but pawns at different coordinates).
    Testing was done utilizing glass box methodology because we were primarily
    concerned with ensuring that the internals functioned properly, since we
    were recreating chess, simply playing the game without experiencing error
    would be the equivalent of black box testing. The files tested were Board,
    Game, and RuleSet. We believe that our test suite demonstrates the
    effectiveness of our system because we were able to achieve good coverage on
    core game functionality that revealed multiple bugs in our implementation.
    Utilizing the code coverage in Bisect, we were able to find where functions
    were not executing as planned and fix the problems relatively quickly. We
    did not choose to specifically omit anything related to game logic from our
    testing but more intensive functions, such as functions around piece moving,
    were tested more extensively than simpler, more straightforward functions.
    We chose to test our TUI implementation by actually using it rather than
    writing test cases for it, since there isn't much reason to test interfaces
    with test cases. We did a great deal of manual testing utilizing our TUI
    implementation. Instead of writing test cases for every possible scenario,
    utilizing functionality in the TUI like highlighting potential moves for the
    user was able to show that our logic for piece movement and game
    functionality was working correctly and was used to validate that we had
    fixed errors identified with the OUnit suite. *)
(*-----------------------------BOARD TESTS------------------------------------*)
(*Helper functions*)

let default_board =
  "\n\
   rnbqkbnr\n\
   pppppppp\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   PPPPPPPP\n\
   RNBQKBNR" |> string_to_board

let default_board_string =
  "\n\
   rnbqkbnr\n\
   pppppppp\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   PPPPPPPP\n\
   RNBQKBNR"

let pushpawn_board =
  "\n\
   rnbqkbnr\n\
   pppppppp\n\
   ________\n\
   ________\n\
   ________\n\
   P_______\n\
   _PPPPPPP\n\
   RNBQKBNR"

let castle_board =
  "\n\
   rnbqkbnr\n\
   pppppppp\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   PPPPPPPP\n\
   R___K__R"

let ks_castle_board =
  "\n\
   rnbqkbnr\n\
   pppppppp\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   PPPPPPPP\n\
   R____RK_"

let qs_castle_board =
  "\n\
   rnbqkbnr\n\
   pppppppp\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   PPPPPPPP\n\
   __KR___R"

let promotion_board =
  "\n\
   ____k___\n\
   P_______\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   _PPPPPPP\n\
   __KR___R"

let promotion_board2 =
  "\n\
   Q___k___\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   _PPPPPPP\n\
   __KR___R"

(* | "♚" | "♔" | "♛" | "♕" | "♜" | "♖" | "♝" | "♗" | "♞" | "♘" | "♟" | "♙" |
   "_" *)

(*Tests for bisect coverage on board.ml*)

let create_p (piece, color, file, rank) =
  ((file, rank), Board.Occupied { piece_type = piece; color; has_moved = false })

(** [create_e (file, rank)] is the coord and square pair of an empty tile. *)
let create_e (file, rank) = ((file, rank), Board.Empty)

let empty_row rank =
  [
    create_e (1, rank);
    create_e (2, rank);
    create_e (3, rank);
    create_e (4, rank);
    create_e (5, rank);
    create_e (6, rank);
    create_e (7, rank);
    create_e (8, rank);
  ]

let pawn_row color rank =
  [
    create_p (Pawn, color, 1, rank);
    create_p (Pawn, color, 2, rank);
    create_p (Pawn, color, 3, rank);
    create_p (Pawn, color, 4, rank);
    create_p (Pawn, color, 5, rank);
    create_p (Pawn, color, 6, rank);
    create_p (Pawn, color, 7, rank);
    create_p (Pawn, color, 8, rank);
  ]

let normal_last_row color rank =
  [
    create_p (Rook, color, 1, rank);
    create_p (Knight, color, 2, rank);
    create_p (Bishop, color, 3, rank);
    create_p (Queen, color, 4, rank);
    create_p (King, color, 5, rank);
    create_p (Bishop, color, 6, rank);
    create_p (Knight, color, 7, rank);
    create_p (Rook, color, 8, rank);
  ]

let the_default_board =
  normal_last_row White 1 (* -------- Rank 1 -------- *)
  @ pawn_row White 2 (* ------------- Rank 2 -------- *)
  @ empty_row 3 (* ------------------ Rank 3 -------- *)
  @ empty_row 4 (* ------------------ Rank 4 -------- *)
  @ empty_row 5 (* ------------------ Rank 5 -------- *)
  @ empty_row 6 (* ------------------ Rank 6 -------- *)
  @ pawn_row Black 7 (* ------------- Rank 7 -------- *)
  @ normal_last_row Black 8 (* ------ Rank 8 -------- *)

let newDefaultBoard = Board.make the_default_board

let enpBoard =
  empty_row 1 (* -------- Rank 1 -------- *)
  @ pawn_row White 2 (* ------------- Rank 2 -------- *)
  @ empty_row 3 (* ------------------ Rank 3 -------- *)
  @ empty_row 4 (* ------------------ Rank 4 -------- *)
  @ empty_row 5 (* ------------------ Rank 5 -------- *)
  @ empty_row 6 (* ------------------ Rank 6 -------- *)
  @ pawn_row Black 7 (* ------------- Rank 7 -------- *)
  @ empty_row 8 (* ------ Rank 8 -------- *)

let extractGame (game : Game.t legality) =
  match game with
  | Legal x -> x
  | Illegal y -> y

let extractBoard (game : Board.t legality) =
  match game with
  | Legal x -> x
  | Illegal y -> y

let enpBoard1 = Board.make enpBoard
let enpBoard2 = Board.move_and_capture enpBoard1 (1, 2) (1, 4) (1, 4)

let enpBoard3 =
  Board.move_and_capture (extractBoard enpBoard2) (8, 7) (8, 6) (8, 6)

let enpBoard4 =
  Board.move_and_capture (extractBoard enpBoard3) (1, 4) (1, 5) (1, 5)

let enpBoard5 =
  Board.move_and_capture (extractBoard enpBoard4) (2, 7) (2, 5) (2, 5)

let enpBoard6 =
  Board.move_and_capture (extractBoard enpBoard5) (1, 5) (2, 4) (2, 5)
(*pawn (1,5) captures pawn at (2,5) by moving to emptpy (2,4)*)

let movedBoard =
  match Board.move_and_capture newDefaultBoard (1, 2) (1, 4) (1, 4) with
  | Legal board -> board
  | _ -> failwith "Error"

let movedBoard2 =
  match Board.move_and_capture newDefaultBoard (1, 2) (1, 3) (1, 3) with
  | Legal board -> Legal board
  | Illegal board -> Illegal board

let test_last_move (name : string) (board : Board.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Board.last_move board)

let test_did_pawn_move2 (name : string) (board : Board.t) expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (Board.did_pawn_move2 board)

let test_turn_counter (name : string) (board : Board.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Board.turn_counter board)

let test_move_and_capture (name : string) (board : Board.t) (start : int * int)
    (dest : int * int) (capture : int * int) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.move_and_capture board start dest capture)

let test_move_and_capture_string (name : string) (board : Board.t)
    (start : int * int) (dest : int * int) (capture : int * int) expected_output
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (board_to_string
       (extractBoard (Board.move_and_capture board start dest capture)))

let test_promote (name : string) (board : Board.t) (coord : int * int)
    promote_to expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.promote board coord promote_to)

let test_inspect (name : string) (board : Board.t) (coord : int * int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Board.inspect board coord)

let defaultRules = RuleSet.initialize_normal_chess
let pawnRules = RuleSet.initialize_pawn_chess

let test_rs_moves (name : string) (rule_set : RuleSet.t) (board : Board.t)
    (coord : int * int) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.length (RuleSet.moves rule_set board coord))

let test_winner (name : string) (rule_set : RuleSet.t) (board : Board.t)
    (color : color) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (RuleSet.winner rule_set board)

let test_board_to_string (name : string) (board : Board.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Board.board_to_string board)

let test_string_to_board (name : string) (board : string) expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (Board.string_to_board board)

let test_piece_to_string (name : string) (piece : chesspiece) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Board.piece_to_string piece)

let test_square_to_string (name : string) (square : square) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Board.square_to_string square)

let test_string_to_square (name : string) (str : string) expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (Board.string_to_square str)

let newGame = Game.make defaultRules
let pawnGame = Game.make pawnRules

let test_inspect_game (name : string) (game : Game.t) (coord : int * int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Game.inspect game coord)

let test_legal_moves (name : string) (game : Game.t) (coord : int * int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Game.legal_moves game coord)

let test_move_game (name : string) (game : Game.t) (start : coord)
    (dest : coord) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (board_to_string (Game.giveBoard (extractGame (Game.move game start dest))))

let test_winner_game (name : string) (game : Game.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Game.winner game)

let test_promotion_available (name : string) (game : Game.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Game.promotion_available game)

let bisecttests =
  "bisect test suite"
  >::: [
         (*Start board.ml tests*)
         test_last_move "initial board" newDefaultBoard None;
         test_did_pawn_move2 "initial board" newDefaultBoard false;
         test_did_pawn_move2 "moved board" movedBoard true;
         test_turn_counter "initial board" newDefaultBoard 0;
         test_turn_counter "moved board" movedBoard 1;
         test_turn_counter "enp board" (extractBoard enpBoard6) 5;
         test_move_and_capture "illegal" newDefaultBoard (1, 2) (0, 0) (0, 0)
           (Illegal newDefaultBoard);
         test_move_and_capture "illegal" (extractBoard enpBoard6) (1, 1) (2, 1)
           (3, 1)
           (Illegal (extractBoard enpBoard6));
         test_move_and_capture_string "qs castle"
           (string_to_board castle_board)
           (5, 1) (3, 1) (3, 1)
           (board_to_string (string_to_board qs_castle_board));
         test_move_and_capture_string "ks castle"
           (string_to_board castle_board)
           (5, 1) (7, 1) (7, 1)
           (board_to_string (string_to_board ks_castle_board));
         test_move_and_capture "legal pawn push" newDefaultBoard (1, 2) (1, 3)
           (1, 3) movedBoard2;
         test_move_and_capture "illegal out of bounds" newDefaultBoard (1, 2)
           (12, 31) (12, 31) (Illegal newDefaultBoard);
         test_move_and_capture "en passant" (extractBoard enpBoard5) (1, 5)
           (2, 4) (2, 5) enpBoard6;
         test_promote "illegal promotion" newDefaultBoard (1, 6) Pawn
           (Illegal newDefaultBoard);
         test_promote "illegal promotion" newDefaultBoard (1, 3) Pawn
           (Illegal newDefaultBoard);
         test_inspect "None" newDefaultBoard (0, 0) None;
         test_board_to_string "standard bts" newDefaultBoard
           "\n\
            rnbqkbnr\n\
            pppppppp\n\
            ________\n\
            ________\n\
            ________\n\
            ________\n\
            PPPPPPPP\n\
            RNBQKBNR";
         test_string_to_board "standard stb"
           "\n\
            rnbqkbnr\n\
            pppppppp\n\
            ________\n\
            ________\n\
            ________\n\
            ________\n\
            PPPPPPPP\n\
            RNBQKBNR"
           default_board;
         test_piece_to_string "pawn"
           { piece_type = Pawn; color = Black; has_moved = false }
           "p";
         test_piece_to_string "rook"
           { piece_type = Rook; color = Black; has_moved = false }
           "r";
         test_piece_to_string "king"
           { piece_type = King; color = Black; has_moved = false }
           "k";
         test_square_to_string "pawn"
           (Occupied { piece_type = Pawn; color = Black; has_moved = false })
           "p";
         test_string_to_square "pawn" "p"
           (Occupied { piece_type = Pawn; color = Black; has_moved = false });
         test_string_to_square "rook" "r"
           (Occupied { piece_type = Rook; color = Black; has_moved = false });
         test_string_to_square "king" "k"
           (Occupied { piece_type = King; color = Black; has_moved = false });
         (*RuleSet tests*)
         test_rs_moves "rook" defaultRules newDefaultBoard (1, 1) 0;
         test_rs_moves "knight" defaultRules newDefaultBoard (2, 1) 2;
         test_rs_moves "bishop" defaultRules newDefaultBoard (3, 1) 0;
         test_rs_moves "queen" defaultRules newDefaultBoard (4, 1) 0;
         test_rs_moves "king" defaultRules newDefaultBoard (5, 1) 0;
         test_rs_moves "bishop" defaultRules newDefaultBoard (6, 1) 0;
         test_rs_moves "knight" defaultRules newDefaultBoard (7, 1) 2;
         test_rs_moves "rook" defaultRules newDefaultBoard (8, 1) 0;
         test_rs_moves "rook" defaultRules newDefaultBoard (1, 8) 0;
         test_rs_moves "knight" defaultRules newDefaultBoard (2, 8) 2;
         test_rs_moves "bishop" defaultRules newDefaultBoard (3, 8) 0;
         test_rs_moves "queen" defaultRules newDefaultBoard (4, 8) 0;
         test_rs_moves "king" defaultRules newDefaultBoard (5, 8) 0;
         test_rs_moves "bishop" defaultRules newDefaultBoard (6, 8) 0;
         test_rs_moves "knight" defaultRules newDefaultBoard (7, 8) 2;
         test_rs_moves "rook" defaultRules newDefaultBoard (8, 8) 0;
         (*----This is every pawn----*)
         test_rs_moves "wpawn" defaultRules newDefaultBoard (1, 2) 2;
         test_rs_moves "wpawn" defaultRules newDefaultBoard (2, 2) 2;
         test_rs_moves "wpawn" defaultRules newDefaultBoard (3, 2) 2;
         test_rs_moves "wpawn" defaultRules newDefaultBoard (4, 2) 2;
         test_rs_moves "wpawn" defaultRules newDefaultBoard (5, 2) 2;
         test_rs_moves "wpawn" defaultRules newDefaultBoard (6, 2) 2;
         test_rs_moves "wpawn" defaultRules newDefaultBoard (7, 2) 2;
         test_rs_moves "wpawn" defaultRules newDefaultBoard (8, 2) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (1, 7) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (2, 7) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (3, 7) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (4, 7) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (5, 7) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (6, 7) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (7, 7) 2;
         test_rs_moves "bpawn" defaultRules newDefaultBoard (8, 7) 2;
         (*----End every pawn----*)
         test_winner "normal chess" defaultRules newDefaultBoard White Ongoing;
         test_winner "en passant battle" defaultRules (extractBoard enpBoard6)
           Black Ongoing;
         (*RuleSet pawn chess tests*)
         test_rs_moves "corner pawn" pawnRules newDefaultBoard (1, 1) 0;
         test_rs_moves "corner pawn" pawnRules newDefaultBoard (8, 1) 0;
         test_rs_moves "pawn" pawnRules newDefaultBoard (1, 2) 2;
         test_rs_moves "pawn" pawnRules newDefaultBoard (8, 2) 2;
         test_winner "pawn chess" pawnRules newDefaultBoard White Ongoing;
         test_winner "en passant battle different RuleSet" pawnRules
           (extractBoard enpBoard6) Black Ongoing;
         (*Start Game.ml tests*)
         test_inspect_game "empty" newGame (1, 5) Empty;
         test_inspect_game "empty (bad square)" newGame (0, 0) Empty;
         test_legal_moves "rook" newGame (1, 1) [];
         test_legal_moves "rook" newGame (1, 8) [];
         test_legal_moves "knight" newGame (2, 1) [ (3, 3); (1, 3) ];
         test_legal_moves "bishop" newGame (3, 1) [];
         test_legal_moves "queen" newGame (4, 1) [];
         test_legal_moves "king" newGame (5, 1) [];
         test_legal_moves "bishop" newGame (6, 1) [];
         test_legal_moves "knight" newGame (7, 1) [ (8, 3); (6, 3) ];
         test_legal_moves "rook" newGame (8, 1) [];
         test_legal_moves "pawn" newGame (1, 2) [ (1, 3); (1, 4) ];
         test_legal_moves "knight black initial turn" newGame (2, 8) [];
         test_legal_moves "bishop black initial turn" newGame (3, 8) [];
         test_legal_moves "queen black initial turn" newGame (4, 8) [];
         test_legal_moves "king black initial turn" newGame (5, 8) [];
         test_legal_moves "bishop black initial turn" newGame (6, 8) [];
         test_legal_moves "knight black initial turn" newGame (7, 8) [];
         test_legal_moves "rook black initial turn" newGame (8, 8) [];
         test_legal_moves "pawn black initial turn" newGame (1, 7) [];
         test_legal_moves "other than piece" newGame (0, 0) [];
         test_winner_game "default board" newGame Ongoing;
         test_winner_game "pawn chess" pawnGame Ongoing;
         test_promotion_available "newGame" newGame None;
         test_promotion_available "pawn game" pawnGame None;
         test_move_game "push pawn" newGame (1, 2) (1, 3) pushpawn_board;
         test_move_game "push pawn illegally" newGame (1, 2) (1, 7)
           default_board_string;
       ]

let _ = run_test_tt_main bisecttests