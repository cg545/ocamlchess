(** Represents a game of chess with a specified rule variation. Serves as the
    point of interaction for any user interfaces.*)

open General

type t

(** The information needed for a user interface to output a tile to the user. *)
type tile_to_draw =
  | Empty
  | Occupied of piece_type * color

val make : RuleSet.t -> t
(** [make r] is a new game whose initialization, legal moves, and win condition
    are specificed by [r]. *)

val giveBoard : t -> Board.t
(**[giveBoard game] is the board used to create the game. *)

val inspect : t -> coord -> tile_to_draw
(** [inspect game coord] is the information a user interface needs to represent
    the tile at [coord] to the user. *)

val legal_moves : t -> coord -> coord list
(** [legal_moves game coord] is the list of the all possible coordinates the
    piece at [coord] can move to. If the specified square is empty, an empty
    list is be returned. Note that this function is intended only to be used for
    highlighting legal moves to the user and should {b not} be used to verify
    the legality of such moves internally. *)

val move : t -> coord -> coord -> t legality
(** [move game start dest] is the [legality] of the piece at [start] moving to
    [dest]. Is [Illegal game] when the move would violate the legal move
    restrictions specified by the game's rule set or if [promotion_available] is
    [true]. Is [Legal game'] when the move produces a legal game state, game'.*)

val winner : t -> winner
(** [winner game] is the [winner] of the game. *)

val promotion_available : t -> coord option
(** [promotions_available game] is [None] if no promotion is available;
    otherwise it is [Some coord] where [coord] is the [coord] of the available
    promotion. *)

val promote : t -> coord -> piece_type -> t legality
(** [promote game coord promote_to] is the [legality] of promoting a pawn at
    [coord] to the type specified by [promote_to]. *)

val turn : t -> color
(** The color of the current player's turn. *)
