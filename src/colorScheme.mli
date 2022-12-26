(** Provides functionality for working with sets of ANSI color escape codes *)

open General

type t
(** Abstract type *)

val background : t -> coord -> coord option -> coord list -> string
(** [background color_scheme this_coord selected_coord highlighted_coords] is
    the string that contains all necessary ANSI color codes to print the
    appropriate background for the visual representation of [this_coord] in
    accourdance with the [color_scheme]. The [selected_coord] represents the
    coord selected by the user to view the legal moves from. The
    [highlighted_coords] represents the coords that are highlighted as legal
    moves.*)

val piece :
  t -> piece_type * color -> coord -> coord option -> coord list -> string
(** [piece color_scheme (piece_type, color) this_coord selected_coord highlighted_coords]
    is the string that contains all necessary information to print the
    foreground of the tile at [this_coord] occupied by a piece of [piece_type]
    with [piece_color]. The [selected_coord] is the [Some] variant when the user
    has selected a coord to view the legal moves from. The [highlighed_coords]
    are where the selected coord can legally move.*)

val neutral_text : t -> string
(** Provides all styling escape codes necessary to print text that is not
    relevant to a specific player*)

val black_text : t -> string
(** Provides all styling escape codes necessary to print text that is relevant
    to the black player, such as displaying that black has won*)

val white_text : t -> string
(** Provides all styling escape codes necessary to print text that is relevant
    to the white player, such as displaying that white has won*)

val color_schemes : (string * t) list
(** An association list between the name of a color scheme and a color scheme.*)

val file_rank_label : int -> t -> string
(** Provides all styling escape codes necessary to print the file and rank
    labels. The supplied int is the numeric representation of the rank/file.*)
