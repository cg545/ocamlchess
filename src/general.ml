(** Definitions of broadly useful types. *)

type coord = int * int
(** Defines a coordinate of a chessboard where the ranks are defined as is
    standard and the files are the numeric conversion of the standard letter
    naming of chess files (A = 1, B = 2, etc..). Note that these coordinates are
    1-based. File is the first int and then rank is the second int. *)

(** Defines the different names of chess pieces. *)
type piece_type =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

(** Defines the color of players. *)
type color =
  | Black
  | White

(** The potential states of a game as either ongoing, stalemated, or won by
    black or white*)
type winner =
  | Ongoing
  | Stalemate
  | Winner of color

(** Represents the state of ['a] after a request to produce a new state of ['a]
    that could potentially be invalid. If the request was valid, the [Legal]
    variant is used containing the state as updated by the request. Otherwise
    the [Illegal] variant is used with the original, unupdated value of ['a].*)
type 'a legality =
  | Illegal of 'a
  | Legal of 'a

(** Creates a list of numbers ascending from the lower number to the upper
    number. Requires: upper >= lower.*)
let rec ascend lower upper =
  if lower < upper then lower :: ascend (lower + 1) upper else [ lower ]

(** Creates a list of numbers descending from the upper number to the lower
    number. Requires: upper >= lower.*)
let rec descend lower upper =
  if lower < upper then upper :: descend lower (upper - 1) else [ upper ]

(** Creates a list of numbers from first to second. The list will be ascending
    if first is less than second. It will be descending if first is greater than
    second.*)
let ( -- ) first second =
  if first < second then ascend first second else descend second first

(** [expand_string str] is a list of all 1-length substrings of str in order of
    appearance*)
let expand_string str =
  List.init (String.length str) (fun index -> String.sub str index 1)
