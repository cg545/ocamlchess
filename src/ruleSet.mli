(** Defines a ruleset for a chess game. A ruleset is a combination of

    - a win condition that determines whether the game is ongoing, stalemated,
      or won by one color
    - movement limitations that may further limit the way pieces can be moved
      from the standard rules of chess
    - an initial state the defines the starting arrangement of the board *)

open General

type t

val sets_of_rules : (string * t) list
(** [sets_of_rules] are the possible rulesets to play a game of chess with. *)

type win_condition = Board.t -> winner
(** A function that analyzes a board and determines which variant of [winner]
    describes it. *)

type movement_restriction = Board.t -> coord -> coord list
(** A function that analyzes a board, a specific square of that board, and
    yields a subset of those potential destinations (the subset may be identical
    to the original set). *)

val make : win_condition -> movement_restriction -> Board.t -> t
(** [make win_condition movement_restrictions initial] is the ruleset that

    {ol
     {- initializes the board to [initial] }
     {- imposes
        movement
        constraints
        in
        addition
        to
        the
        universal
        movement
        constraints,
        which
        are

        + Pieces move in the shape that they do in standard chess
        + Pieces cannot move through each other except for knights and a rook
          when castling
        + Pieces cannot move off the board
        + A player may not move themselves in such a way that they are in check
          after the move. This includes moving through check when castling
        + No moves can be made if the game is concluded
        + Pawns can only move two spaces if they have not previously moved
        + Pawns can only move diagonally if there is an opposing piece in that
          position or for an en passant capture
        + Pieces may only move onto occupied squares if that square is occupied
          by an opposing color and that capture movement is legal
        + The king may move two spaces to either side if it has not previously
          moved, the rook to that side has not moved, the king does not pass
          through check in doing so, and does not have any pieces between itself
          and the rook. This motion automatically moves the rook.
     }
     {- defines
        a
        win
        condition
        that
        is
        checked
        after
        the
        universal
        win
        conditions
        are
        checked,
        which
        are

        + The game is a stalemate if the current player has no legal moves and
          are not in check.
     }
    } *)

val initial_board : t -> Board.t
(** The initial configuration of the board. *)

val moves : t -> Board.t -> coord -> coord list
(** [moves rule_set board coord] is the list of legal moves that can be made
    from coord given [rule_set]'s movement constraints (in addition to universal
    movement constraints), or the empty list if no legal moves can be made from
    coord. *)

val initialize_normal_chess : t
val initialize_pawn_chess : t

val winner : t -> Board.t -> winner
(** [winner rule_set board] is the winner of the game as determined by
    [rule_set]'s win conditions (in addition to universal win conditions).*)
