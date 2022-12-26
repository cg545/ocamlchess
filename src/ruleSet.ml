open General

type win_condition = Board.t -> winner
type movement_restriction = Board.t -> coord -> coord list

type t = {
  win_condition : win_condition;
  movement_restriction : movement_restriction;
  initial : Board.t;
}

let make win_c movement_restrict board =
  {
    win_condition = win_c;
    movement_restriction = movement_restrict;
    initial = board;
  }

let initial_board rule_set = rule_set.initial

(** [confines_checker (f, r)] checks to make sure that [f] ad [r] is a
    coordinate of the board as a file and row can be [1 - 8]*)
let confines_checker (f, r) = f >= 1 && f <= 8 && r >= 1 && r <= 8

(** [board_parser_rank filt board color] checks if current rank is out of
    bounds, and then checks each square in file [curr_f] of rank [curr_r] with
    [filt]*)
let rec board_parser_rank filt board color curr_f curr_r =
  match Board.inspect board (curr_f, curr_r) with
  | None -> (false, (-1, -1))
  | Some (Board.Occupied p) when filt p curr_f curr_r -> (true, (curr_f, curr_r))
  | _ -> board_parser_rank filt board color curr_f (curr_r + 1)

(** [board_parser_file filt board color] checks if current file is out of
    bounds, and then calls board_parser_rank to parse current file. *)
let rec board_parser_file filt board color curr_f =
  match Board.inspect board (curr_f, 1) with
  | None -> None
  | Some _ -> begin
      match board_parser_rank filt board color curr_f 1 with
      | true, coord -> Some coord
      | false, _ -> board_parser_file filt board color (curr_f + 1)
    end

(** [board_parser filt board color] returns a Some coord for the first square
    that satisfies filt. Returns None if no such square exists *)
let board_parser filt board color = board_parser_file filt board color 1

(** [square_of_king board color] is the coordinate option of the king of color
    [color] in board [board]. None if there is no king present of [color] *)
let square_of_king board color =
  board_parser (fun p f r -> p.piece_type = King && p.color = color) board color

(** [is_oppo_piece chessboard color (file, rank)] is the validity of the fact
    that at coordinate [(file, rank)] on board [chessboard], there is a piece
    that is not of color [color]*)
let is_oppo_piece chessboard color (file, rank) =
  match Board.inspect chessboard (file, rank) with
  | Some (Occupied p) when p.Board.color <> color -> true
  | _ -> false

(** This is the helper for [king_not_in_check]. Each piece's position is passed
    in as well as their movement pattern. For pieces that can move more than one
    square at a time, this function increments along each direction that is
    passed in making sure to note if there is a piece blocking the piece's path.
    This function generates a list of all coordinates of pieces that are
    checking the specificed king.*)
let rec in_check_helper piece_type opp_king_color board s_f s_r change_f
    change_r acc curr_f curr_r =
  let curr_square = Board.inspect board (curr_f, curr_r) in
  match curr_square with
  | None -> acc
  | Some Board.Empty ->
      in_check_helper piece_type opp_king_color board s_f s_r change_f change_r
        acc (curr_f + change_f) (curr_r + change_r)
  | Some (Board.Occupied curr_p) ->
      if curr_p.color <> opp_king_color && curr_p.piece_type = piece_type then
        (curr_f, curr_r) :: acc
      else acc

(** For knights: They have a set of 8 possible moves. No piece can block them
    from giving check, so each move away from king's square must be checked.*)
let knight_list file rank =
  [
    (file + 2, rank + 1);
    (file + 2, rank - 1);
    (file - 2, rank + 1);
    (file - 2, rank - 1);
    (file + 1, rank + 2);
    (file - 1, rank + 2);
    (file + 1, rank - 2);
    (file - 1, rank - 2);
  ]

(** For bishops: They move on two diagonals and their checks can be blocked. So
    every square on the diagonals away from the king should be checked until
    another piece is present.*)
let bishop_list func p opp_king_color chessboard file rank =
  List.flatten
    [
      func p opp_king_color chessboard file rank 1 1 [] (file + 1) (rank + 1);
      func p opp_king_color chessboard file rank (-1) 1 [] (file - 1) (rank + 1);
      func p opp_king_color chessboard file rank 1 (-1) [] (file + 1) (rank - 1);
      func p opp_king_color chessboard file rank (-1) (-1) [] (file - 1)
        (rank - 1);
    ]

(** For rooks: The move on two horizontals and their checks can be blocked. So
    every square on the row and file away from the king should be checked until
    another piece is present.*)
let rook_list func p opp_king_color chessboard file rank =
  List.flatten
    [
      func p opp_king_color chessboard file rank 1 0 [] (file + 1) rank;
      func p opp_king_color chessboard file rank (-1) 0 [] (file - 1) rank;
      func p opp_king_color chessboard file rank 0 1 [] file (rank + 1);
      func p opp_king_color chessboard file rank 0 (-1) [] file (rank - 1);
    ]

(** For queens: Combine search used for bishops and rooks. *)
let queen_list func p opp_king_color chessboard file rank =
  List.flatten
    [
      func p opp_king_color chessboard file rank 1 1 [] (file + 1) (rank + 1);
      func p opp_king_color chessboard file rank (-1) 1 [] (file - 1) (rank + 1);
      func p opp_king_color chessboard file rank 1 (-1) [] (file + 1) (rank - 1);
      func p opp_king_color chessboard file rank (-1) (-1) [] (file - 1)
        (rank - 1);
      func p opp_king_color chessboard file rank 1 0 [] (file + 1) rank;
      func p opp_king_color chessboard file rank (-1) 0 [] (file - 1) rank;
      func p opp_king_color chessboard file rank 0 1 [] file (rank + 1);
      func p opp_king_color chessboard file rank 0 (-1) [] file (rank - 1);
    ]

(** For pawns: There are only two squares that a pawn can check a king and
    cannot be blocked, so they are checked.*)
let pawn_list opp_king_color file rank =
  if opp_king_color = White then [ (file - 1, rank - 1); (file + 1, rank - 1) ]
  else [ (file - 1, rank + 1); (file + 1, rank + 1) ]

(** For kings: There are only eight squares that a king can "check" a king and
    cannot be blocked, so they are checked. Kings technically cannot check other
    kings, but a king cannot be one square away from another king, so it is
    though it would be moving into check. *)
let king_list file rank =
  [
    (file + 1, rank);
    (file + 1, rank + 1);
    (file, rank + 1);
    (file - 1, rank + 1);
    (file - 1, rank);
    (file - 1, rank - 1);
    (file, rank - 1);
    (file + 1, rank - 1);
  ]

(** [check_filt lst board coord p_t opp_king_color] filters [lst] based on if a
    king at [coord] of color [opp_king_color] is in check by a piece of type
    [p_t] on board [board]*)
let check_filt lst board coord p_t opp_king_color =
  List.filter
    (fun coord ->
      match Board.inspect board coord with
      | Some (Occupied p) when p.piece_type = p_t && p.color <> opp_king_color
        -> true
      | _ -> false)
    lst

(** [k_p_k_checking piece the_list chessboard file rank king_color] is the
    validity of the statement that a king of color [king_color] at
    [(file, rank)] is in check by a piece of type [piece] (which must be a
    Knight, Pawn, or King) that moves according to [the_list] on board
    [chessboard]*)
let k_p_k_checking piece the_list chessboard file rank king_color =
  let the_checks = the_list file rank in
  if
    List.length (check_filt the_checks chessboard (file, rank) piece king_color)
    <> 0
  then true
  else false

(** [b_r_q_checking piece the_list chessboard file rank king_color] is the
    validity of the statement that a king of color [king_color] at
    [(file, rank)] is in check by a piece of type [piece] (which must be a
    Bishop, Rook, or Queen) that moves according to [the_list] on board
    [chessboard]*)
let b_r_q_checking piece the_list chessboard file rank king_color =
  let the_checks =
    the_list in_check_helper piece king_color chessboard file rank
  in
  if List.length the_checks <> 0 then true else false

(** [king_not_in_check chessboard king_c] returns the validity of the fact that
    the king of color [king_c] is not in check on the board [board]. This is
    done by checking all possible moves by opposing colored pieces.*)
let king_not_in_check chessboard king_c =
  let king_square = square_of_king chessboard king_c in
  match king_square with
  | None -> true
  | Some (file, rank) ->
      if k_p_k_checking Knight knight_list chessboard file rank king_c then
        false
      else if b_r_q_checking Bishop bishop_list chessboard file rank king_c then
        false
      else if b_r_q_checking Rook rook_list chessboard file rank king_c then
        false
      else if b_r_q_checking Queen queen_list chessboard file rank king_c then
        false
      else if k_p_k_checking Pawn (pawn_list king_c) chessboard file rank king_c
      then false
      else if k_p_k_checking King king_list chessboard file rank king_c then
        false
      else true

(** [valid_move board (s_f, s_r) (e_f, e_r)] returns the validity of moving a
    piece at [(s_f, s_r)] to [e_f, e_r)] on board [board].*)
let valid_move board (s_f, s_r) (e_f, e_r) =
  let start_square = Board.inspect board (s_f, s_r) in
  match start_square with
  | None -> false
  | Some Empty -> false
  | Some (Occupied piece) -> (
      let target_square = Board.inspect board (e_f, e_r) in
      match target_square with
      | None -> false
      | Some (Board.Occupied p) when p.Board.color = piece.Board.color -> false
      | Some _ -> (
          let after_movement =
            Board.move_and_capture board (s_f, s_r) (e_f, e_r) (e_f, e_r)
          in
          match after_movement with
          | Illegal b -> false
          | Legal b -> king_not_in_check b piece.Board.color))

(** Similar to [in_check_helper] but here the starting file and rank is that of
    the selected piece and this returns all the possible squares that piece can
    move to from its current position.*)
let rec move_helper p _ board s_f s_r change_f change_r acc curr_f curr_r =
  let curr_square = Board.inspect board (curr_f, curr_r) in
  match curr_square with
  | None -> acc
  | Some Board.Empty ->
      if valid_move board (s_f, s_r) (curr_f, curr_r) then
        move_helper p () board s_f s_r change_f change_r
          ((curr_f, curr_r) :: acc) (curr_f + change_f) (curr_r + change_r)
      else
        move_helper p () board s_f s_r change_f change_r acc (curr_f + change_f)
          (curr_r + change_r)
  | Some (Board.Occupied curr_p) ->
      if valid_move board (s_f, s_r) (curr_f, curr_r) then
        (curr_f, curr_r) :: acc
      else acc

(** [pawn_left_or_right board piece file rank] the validity of the statement
    that the pawn at [(file, rank)] can be captured through en passant by piece
    [piece] on board [board]. *)
let pawn_left_or_right board piece file rank =
  match Board.inspect board (file, rank) with
  | None -> false
  | Some Board.Empty -> false
  | Some (Board.Occupied p) ->
      Board.last_move board = Some ((file, rank), Board.Occupied p)
      && p.Board.piece_type = Pawn && Board.did_pawn_move2 board
      && p.Board.color <> piece.Board.color

(** [can_en_passant board piece file rank] the pair of truths that describe if
    the pawn on the laeft adn right can be taken by en passant by piece [piece]
    on board [board]: (left pawn, right pawn). *)
let can_en_passant board piece file rank =
  if
    (piece.Board.color = White && rank <> 5)
    || (piece.color = Black && rank <> 4)
  then (false, false)
  else
    ( pawn_left_or_right board piece (file - 1) rank,
      pawn_left_or_right board piece (file + 1) rank )

(** [inspect_pawn_mov color chessboard curr_f curr_f file rank] is the list
    containing the coordinates to where the pawn at [(curr_f, curr_r)] can move
    to without capturing (without taking into account the king being left in
    check). *)
let inspect_pawn_mov chessboard curr_f curr_f file rank =
  match Board.inspect chessboard (file, rank) with
  | Some Empty -> [ (file, rank) ]
  | _ -> []

(** [pawn_movement piece file rank] the list of the coordinates that the Pawn
    [piece] located at [(file, rank)] can move to on board [chessboard] through
    regular movement. *)
let pawn_movement piece chessboard file rank =
  if piece.Board.color = White then
    inspect_pawn_mov chessboard file rank file (rank + 1)
    @
    if not piece.has_moved then
      inspect_pawn_mov chessboard file rank file (rank + 2)
    else []
  else
    inspect_pawn_mov chessboard file rank file (rank - 1)
    @
    if not piece.has_moved then
      inspect_pawn_mov chessboard file rank file (rank - 2)
    else []

(** [inspect_pawn_capt color chessboard curr_f curr_r file rank] is the list
    containing [(file, rank)] if the pawn at [(curr_f, curr_r)] can capture a
    piece at [(file, rank)] (without taking into account the king being left in
    check). *)
let inspect_pawn_capt color chessboard curr_f curr_r file rank =
  match Board.inspect chessboard (file, rank) with
  | Some (Occupied p_diff_c) when is_oppo_piece chessboard color (file, rank) ->
      if valid_move chessboard (curr_f, curr_r) (file, rank) then
        [ (file, rank) ]
      else []
  | _ -> []

(** [with_reg_capture piece chessboard file rank] is the list of the coordinates
    that the Pawn [piece] located at [(file, rank)] can move to on board
    [chessboard] through capturing (if it even can). *)
let with_reg_capture piece chessboard file rank =
  if piece.Board.color = White then
    inspect_pawn_capt piece.color chessboard file rank (file - 1) (rank + 1)
    @ inspect_pawn_capt piece.color chessboard file rank (file + 1) (rank + 1)
  else
    inspect_pawn_capt piece.color chessboard file rank (file - 1) (rank - 1)
    @ inspect_pawn_capt piece.color chessboard file rank (file + 1) (rank - 1)

(** [with_en_passant_moves piece chessboard file rank] the list of the
    coordinates that the Pawn [piece] located at [(file, rank)] can move to on
    board [chessboard] through en passant (if it even can). *)
let with_en_passant_moves piece chessboard file rank =
  if piece.Board.color = General.White then
    match can_en_passant chessboard piece file rank with
    | false, false -> []
    | true, false -> [ ((file - 1, rank + 1), (file - 1, rank)) ]
    | false, true -> [ ((file + 1, rank + 1), (file + 1, rank)) ]
    | true, true ->
        [
          ((file - 1, rank + 1), (file - 1, rank));
          ((file + 1, rank + 1), (file + 1, rank));
        ]
  else
    match can_en_passant chessboard piece file rank with
    | false, false -> []
    | true, false -> [ ((file - 1, rank - 1), (file - 1, rank)) ]
    | false, true -> [ ((file + 1, rank - 1), (file + 1, rank)) ]
    | true, true ->
        [
          ((file - 1, rank - 1), (file - 1, rank));
          ((file + 1, rank - 1), (file + 1, rank));
        ]

(** [valid_enpassant board (s_f, s_r) (e_f, e_r) (c_f, c_r)] returns the
    validity of moving a piece at [(s_f, s_r)] to [e_f, e_r)] on board [board].*)
let valid_enpassant board (s_f, s_r) ((e_f, e_r), (c_f, c_r)) =
  let start_square = Board.inspect board (s_f, s_r) in
  match start_square with
  | None -> false
  | Some Empty -> false
  | Some (Occupied piece) -> (
      let target_square = Board.inspect board (e_f, e_r) in
      match target_square with
      | None -> false
      | Some _ -> (
          let after_movement =
            Board.move_and_capture board (s_f, s_r) (e_f, e_r) (c_f, c_r)
          in
          match after_movement with
          | Illegal b -> false
          | Legal b -> king_not_in_check b piece.Board.color))

(** [pawn_move chessboard piece file rank] the list of the coordinates that the
    Pawn [piece] located at [(file, rank)] can move to on board [chessboard].
    This includes capturing and en passant movement. *)
let pawn_move chessboard piece file rank =
  let en_pass_pair_coord =
    List.filter
      (valid_enpassant chessboard (file, rank))
      (with_en_passant_moves piece chessboard file rank)
  in
  let en_pass_coord =
    List.fold_left
      (fun acc (e_pair, c_pair) -> e_pair :: acc)
      [] en_pass_pair_coord
  in
  List.filter
    (valid_move chessboard (file, rank))
    (pawn_movement piece chessboard file rank
    @ with_reg_capture piece chessboard file rank)
  @ en_pass_coord

(** [knight_move chessboard piece file rank] the list of the coordinates that
    the Knight [piece] located at [(file, rank)] can move to on board
    [chessboard]. *)
let knight_move chessboard piece file rank =
  knight_list file rank |> List.filter (valid_move chessboard (file, rank))

(** [bishop_move chessboard piece file rank] the list of the coordinates that
    the Bishop [piece] located at [(file, rank)] can move to on board
    [chessboard]. *)
let bishop_move chessboard piece file rank =
  bishop_list move_helper piece () chessboard file rank

(** [rook_move chessboard piece file rank] the list of the coordinates that the
    Rook [piece] located at [(file, rank)] can move to on board [chessboard]. *)
let rook_move chessboard piece file rank =
  rook_list move_helper piece () chessboard file rank

(** [queen_move chessboard piece file rank] the list of the coordinates that the
    Queen [piece] located at [(file, rank)] can move to on board [chessboard]. *)
let queen_move chessboard piece file rank =
  queen_list move_helper piece () chessboard file rank

(** [can_castle_rook_check r] is validity of the statement that the piece [r] is
    indeed a Rook and has not moved yet. *)
let can_castle_rook_check r = r.Board.piece_type = Rook && not r.Board.has_moved

(** [queenside_rook color board] is option of the sqaure where the queenside
    rook, if the king of color [color] on board [board] is able to move to each
    of the squares in its path to castle on the queenside. If it cannot then it
    is Some of an empty square. *)
let queenside_rook color board =
  if color = General.White then
    if
      valid_move board (5, 1) (4, 1)
      && valid_move board (5, 1) (3, 1)
      && Board.inspect board (4, 1) = Some Board.Empty
      && Board.inspect board (3, 1) = Some Board.Empty
      && Board.inspect board (2, 1) = Some Board.Empty
    then Board.inspect board (1, 1)
    else Some Board.Empty
  else if
    valid_move board (5, 8) (4, 8)
    && valid_move board (5, 8) (3, 8)
    && Board.inspect board (4, 8) = Some Board.Empty
    && Board.inspect board (3, 8) = Some Board.Empty
    && Board.inspect board (2, 8) = Some Board.Empty
  then Board.inspect board (1, 8)
  else Some Board.Empty

(** [kingside_rook color board] is option of the sqaure where the kingside rook,
    if the king of color [color] on board [board] is able to move to each of the
    squares in its path to castle on the kingside. If it cannot then it is Some
    of an empty square. *)
let kingside_rook color board =
  if color = General.White then
    if
      valid_move board (5, 1) (6, 1)
      && valid_move board (5, 1) (7, 1)
      && Board.inspect board (6, 1) = Some Board.Empty
      && Board.inspect board (7, 1) = Some Board.Empty
    then Board.inspect board (8, 1)
    else Some Board.Empty
  else if
    valid_move board (5, 8) (6, 8)
    && valid_move board (5, 8) (7, 8)
    && Board.inspect board (6, 8) = Some Board.Empty
    && Board.inspect board (7, 8) = Some Board.Empty
  then Board.inspect board (8, 8)
  else Some Board.Empty

(** [can_castle board color] is the validity of the statement that the king of
    color [color] can castle on board [board]. *)
let can_castle board color =
  let king =
    Board.inspect board (if color = General.White then (5, 1) else (5, 8))
  in
  match king with
  | None | Some Board.Empty -> (false, false)
  | Some (Board.Occupied p) when p.Board.piece_type <> King || p.Board.has_moved
    -> (false, false)
  | Some (Board.Occupied k) -> (
      let q_rook = queenside_rook color board in
      let k_rook = kingside_rook color board in
      match (q_rook, k_rook) with
      | None, _ -> (false, false)
      | _, None -> (false, false)
      | Some Board.Empty, Some Board.Empty -> (false, false)
      | Some Board.Empty, Some (Board.Occupied k) ->
          (false, can_castle_rook_check k)
      | Some (Board.Occupied q), Some Board.Empty ->
          (can_castle_rook_check q, false)
      | Some (Board.Occupied q), Some (Board.Occupied k) ->
          (can_castle_rook_check q, can_castle_rook_check k))

(** [king_move chessboard piece file rank] is the list of moves that the King
    [piece] can move. This includes being able to castle. *)
let king_move chessboard piece file rank =
  let king_movement =
    [
      (file + 1, rank);
      (file + 1, rank + 1);
      (file, rank + 1);
      (file - 1, rank + 1);
      (file - 1, rank);
      (file - 1, rank - 1);
      (file, rank - 1);
      (file + 1, rank - 1);
    ]
  in
  let with_castle_moves =
    match can_castle chessboard piece.Board.color with
    | false, false -> []
    | false, true -> [ (file + 2, rank) ]
    | true, false -> [ (file - 2, rank) ]
    | true, true -> [ (file - 2, rank); (file + 2, rank) ]
  in
  List.filter
    (valid_move chessboard (file, rank))
    (king_movement @ with_castle_moves)

(** The movement constraints that are always applied. These are documented in
    [RuleSet.make] (must be documented in public interface so clients know; do
    {b not} document in two places). *)
let universal_movement_constraints chessboard (file, rank) =
  let curr_sq = Board.inspect chessboard (file, rank) in
  match curr_sq with
  | None | Some Board.Empty -> []
  | Some (Board.Occupied p) ->
      (match p.piece_type with
      | King -> king_move
      | Queen -> queen_move
      | Rook -> rook_move
      | Bishop -> bishop_move
      | Knight -> knight_move
      | Pawn -> pawn_move)
        chessboard p file rank

let moves rule_set board (file, rank) =
  rule_set.movement_restriction board (file, rank)

(** [cannot_move board color] is the validity of the statement that the player
    of color [color] cannot make a move on board [board]. *)
let cannot_move board color =
  let any_piece_can_move =
    board_parser
      (fun p f r ->
        p.color = color
        && List.length (universal_movement_constraints board (f, r)) <> 0)
      board color
  in
  match any_piece_can_move with
  | None -> true
  | Some _ -> false

let winner rule_set board = rule_set.win_condition board

(** ------------------------------------------------------------------------- *)

(** ---------------------- Creating Different Rule Sets --------------------- *)

(** ------------------------------------------------------------------------- *)

(** [win_condition_normal board] is the how the winner of a normal game of chess
    is decided. If the white king is in check, and white cannot move, then black
    wins. If the black king is in check, and black cannot move, then white wins.
    If neither king is in check and white or black cannot move during their
    turn, then it is a stalemate. If none of the above are true, then the game
    is still going on. *)
let win_condition_normal board =
  let w_in_check = not (king_not_in_check board White) in
  let w_impossible_to_move = cannot_move board White in
  if w_in_check && w_impossible_to_move then Winner Black
  else
    let b_in_check = not (king_not_in_check board Black) in
    let b_impossible_to_move = cannot_move board Black in
    if b_in_check && b_impossible_to_move then Winner White
    else if
      (Board.turn_counter board mod 2 = 0 && w_impossible_to_move)
      || (Board.turn_counter board mod 2 = 1 && b_impossible_to_move)
    then Stalemate
    else Ongoing

(** [any_pieces_left color board] checks to see if there are any pieces of color
    [color] left on board [board]*)
let any_pieces_left color board =
  match board_parser (fun p f r -> p.color = color) board color with
  | None -> false
  | Some _ -> true

(** [win_condition_pawn board] is the how the winner of a game of pawn chess is
    decided. If white has no more pieces left on the board, then black wins. If
    black has no more pieces left on the board, then white wins. If either white
    or black cannot move during their turn, then it is a stalemate. If none of
    the above are true, then the game is still going on. *)
let win_condition_pawn board =
  let white_piece_exists = any_pieces_left White board in
  if not white_piece_exists then Winner Black
  else
    let black_piece_exists = any_pieces_left Black board in
    if not black_piece_exists then Winner White
    else
      let w_impossible_to_move = cannot_move board White in
      let b_impossible_to_move = cannot_move board Black in
      if
        (Board.turn_counter board mod 2 = 0 && w_impossible_to_move)
        || (Board.turn_counter board mod 2 = 1 && b_impossible_to_move)
      then Stalemate
      else Ongoing

(** [create_p (piece, color, file, rank)] is the coord and square pair with
    piece [piece] of color [color] at that tile. *)
let create_p (piece, color, file, rank) =
  ((file, rank), Board.Occupied { piece_type = piece; color; has_moved = false })

(** [create_e (file, rank)] is the coord and square pair of an empty tile. *)
let create_e (file, rank) = ((file, rank), Board.Empty)

(** [empty_row rank] is row of empty tiles along rank [rank]. *)
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

(** [pawn_row color rank] is row of tiles containg Pawns of color [color] along
    rank [rank]. *)
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

(** [normal_last_row color rank] is row of tiles containg the back row of pieces
    of a normal game of chess of color [color] along rank [rank]. *)
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

(** [the_default_board] is board for a normal game of chess. *)
let the_default_board =
  normal_last_row White 1 (* -------- Rank 1 -------- *)
  @ pawn_row White 2 (* ------------- Rank 2 -------- *)
  @ empty_row 3 (* ------------------ Rank 3 -------- *)
  @ empty_row 4 (* ------------------ Rank 4 -------- *)
  @ empty_row 5 (* ------------------ Rank 5 -------- *)
  @ empty_row 6 (* ------------------ Rank 6 -------- *)
  @ pawn_row Black 7 (* ------------- Rank 7 -------- *)
  @ normal_last_row Black 8 (* ------ Rank 8 -------- *)

(** [the_pawn_board] is board for a game of pawn chess. *)
let the_pawn_board =
  empty_row 1 (*--------------------- Rank 1 -------- *)
  @ pawn_row White 2 (* ------------- Rank 2 -------- *)
  @ empty_row 3 (*------------------- Rank 3 -------- *)
  @ empty_row 4 (* ------------------ Rank 4 -------- *)
  @ empty_row 5 (* ------------------ Rank 5 -------- *)
  @ empty_row 6 (* ------------------ Rank 6 -------- *)
  @ pawn_row Black 7 (* ------------- Rank 7 -------- *)
  @ empty_row 8 (*------------------- Rank 8 -------- *)

(** [initialize_normal_chess] is the ruleset for a normal game of chess. *)
let initialize_normal_chess =
  make win_condition_normal universal_movement_constraints
    (Board.make the_default_board)

(** [initialize_pawn_chess] is the ruleset for a game of pawn chess. *)
let initialize_pawn_chess =
  make win_condition_pawn universal_movement_constraints
    (Board.make the_pawn_board)

let sets_of_rules =
  [
    ("Normal Chess", initialize_normal_chess);
    ("Pawn Chess", initialize_pawn_chess);
  ]
