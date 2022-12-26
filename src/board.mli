(** Defines a chessboard. To ensure maximum generality of rules, the chess board
    treats all moves as valid except for those that would create unrepresentable
    states. These moves are those that

    - exceed the bounds of the 8x8 chessboard, which uses a 1-based coordinate
      system
    - result in 2 pieces occupying the same space without one capturing the
      other
    - attempt to move an empty space somewhere else

    This module should not be accessed directly by a user interface, which
    should instead interface with [Game]. *)

open General

type t
(** Representation of a chessboard with pieces*)

type chesspiece = {
  piece_type : piece_type;
  color : color;
  has_moved : bool;
}

type square =
  | Empty
  | Occupied of chesspiece

val make : (coord * square) list -> t
(** [make lst] is the board defined by association list [lst]. *)

val last_move : t -> (coord * square) option
(** [last_move] is the coordinates option of the last move. *)

val did_pawn_move2 : t -> bool
(** [did_pawn_move2] is the validity of the statement that the last move was a
    pawn moving two squares forward. *)

val move_and_capture : t -> coord -> coord -> coord -> t legality
(** [move_and_capture board start dest capture] is the [legality] of the board
    where the square at [capture] is made empty, and the piece at start [start]
    moves to [dest]. Is [Illegal board] if

    - any coordinates exceed the 8x8 bounds
    - [start] contains an empty square
    - [dest] contains a piece even after the piece at [capture] is removed

    Note that [capture] and [dest] will be the same unless there is an en
    passant capture. *)

val promote : t -> coord -> piece_type -> t legality
(** [promote game location promote_to] is the [legality] of promoting the piece
    at [location] to [promote_to]. Is [Illegal game] if

    - coord is an empty square
    - coord is out of the 8x8 bounds of a chessboard*)

val inspect : t -> coord -> square option
(** [inspect board coord] is the square at [coord]. Is [None] if [coord] is
    outside the 8x8 bounds of a chess board. *)

val turn_counter : t -> int
(** The number of moves made on this board *)
(* Note: beware of not counting castling as two moves! *)

val board_to_string : t -> string
(** Converts a one-character string to its square representation. Lowercase
    strings represent black while uppercase represents white. "_" is regarded as
    [Empty].

    Requires: [str] is a one-character string that is a standard chess
    abbreviation or "_".*)

val string_to_board : string -> t
(** Converts a string to a board. The string must have 8 lines, each
    representing a rank on a chess board. Each line has only 8 characters and
    the newline characters. Non-newline characters are standard chess
    abbreviations or "_", to represent the empty squares. Black pieces are
    represented as lowercase letters while white pieces are uppercase. Leading
    and trailing whitespace is ignored.

    Example: The standard chess configuration is represented as
    ["\n\
   rnbqkbnr\n\
   pppppppp\n\
   ________\n\
   ________\n\
   ________\n\
   ________\n\
   PPPPPPPP\n\
   RNBQKBNR"
    ]*)

val string_to_square : string -> square
(** Converts a one-character string to its square representation. Lowercase
    strings represent black while uppercase represents white. "_" is regarded as
    [Empty].

    Requires: [str] is a one-character string that is a standard chess
    abbreviation or "_".*)

val square_to_string : square -> string
(** Turns occupied squares into the string representation of their occupier
    (where uppercase letters are white and lowercase are black and standard
    chess abbreviations are used) and unoccupied squares into "_"*)

val piece_to_string : chesspiece -> string
(** Turns a piece into a string representation where pieces use their standard
    abbreviations. Black pieces are lowercase while white piece are uppercase*)

val all_coords : coord list list
(** All coordinates in an 8x8 chess game. The top-left is (1, 8) while the
    bottom-right is (8, 1)*)