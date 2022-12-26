(* let ruleset = "Ruleset" *)

(* type square = | Empty | Occup *)

type color =
  | Black
  | White
  | Empty

type coord = int * int

type moving = {
  start : coord;
  dest : coord;
}

type chess_piece =
  | Empty
  | King of color * bool
  | Queen of color * bool
  | Rook of color * bool
  | Bishop of color * bool
  | Knight of color * bool
  | Pawn of color * bool

module type ChessPiece = sig
  type t = chess_piece

  val create : color -> t

  (* val color : t -> color val has_moved : t -> bool *)
  val movement : t -> coord -> coord -> bool
end

module Basic = struct
  type t = chess_piece

  let color = function
    | Pawn (c, _) -> c
    | Rook (c, _) -> c
    | Knight (c, _) -> c
    | Bishop (c, _) -> c
    | Queen (c, _) -> c
    | King (c, _) -> c
    | Empty -> Empty

  (* let has_moved = function | Pawn (_, b) -> b | Rook (_, b) -> b | Knight (_,
     b) -> b | Bishop (_, b) -> b | Queen (_, b) -> b | King (_, b) -> b | Empty
     -> false *)
end

module Pawn : ChessPiece = struct
  include Basic

  let create color = Pawn (color, false)

  let movement piece (c1, r1) (c2, r2) =
    if c1 <> c2 then false
    else if color piece = White then
      if r1 = 6 then r2 - r1 = -2 || r2 - r1 = -1 else r2 - r1 = -1
    else if r1 = 1 then r2 - r1 = 2 || r2 - r1 = 1
    else r2 - r1 = 1
end

module Rook : ChessPiece = struct
  include Basic

  let create color = Rook (color, false)
  let movement piece (c1, r1) (c2, r2) = (piece = piece && c1 = c2) || r1 = r2
end

module Knight : ChessPiece = struct
  include Basic

  let create color = Knight (color, false)

  let movement piece (c1, r1) (c2, r2) =
    (piece = piece && abs (c1 - c2) = 1 && abs (r1 - r2) = 2)
    || (abs (c1 - c2) = 2 && abs (r1 - r2) = 1)
end

module Bishop : ChessPiece = struct
  include Basic

  let create color = Bishop (color, false)

  let movement piece (c1, r1) (c2, r2) =
    piece = piece && abs (c1 - c2) = abs (r1 - r2)
end

module Queen : ChessPiece = struct
  include Basic

  let create color = Queen (color, false)

  let movement piece (c1, r1) (c2, r2) =
    (piece = piece && abs (c1 - c2) = abs (r1 - r2)) || c1 = c2 || r1 = r2
end

module King : ChessPiece = struct
  include Basic

  let create color = King (color, false)

  let movement piece (c1, r1) (c2, r2) =
    piece = piece
    && abs (c1 - c2) + abs (r1 - r2) <= 2
    && abs (c1 - c2) + abs (r1 - r2) > 0
    && abs (c1 - c2) <> 2
    && abs (r1 - r2) <> 2
end

module type ChessBoard = sig
  type t

  val create : t
  val piece_at_square : t -> int -> int -> chess_piece
  val display_board : t -> unit
  val access_square : chess_piece -> t -> int -> int -> t
end

module RegularChessBoard : ChessBoard = struct
  type t = chess_piece list list

  let create =
    let w_p = Pawn.create White in
    let w_r = Rook.create White in
    let w_n = Knight.create White in
    let w_b = Bishop.create White in
    let w_q = Queen.create White in
    let w_k = King.create White in
    let b_p = Pawn.create Black in
    let b_r = Rook.create Black in
    let b_n = Knight.create Black in
    let b_b = Bishop.create Black in
    let b_q = Queen.create Black in
    let b_k = King.create Black in
    [
      [ b_r; b_n; b_b; b_q; b_k; b_b; b_n; b_r ];
      [ b_p; b_p; b_p; b_p; b_p; b_p; b_p; b_p ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ w_p; w_p; w_p; w_p; w_p; w_p; w_p; w_p ];
      [ w_r; w_n; w_b; w_q; w_k; w_b; w_n; w_r ];
    ]

  let rec col_helper row num =
    match row with
    | [] -> Empty
    | h :: t -> if num = 0 then h else col_helper t (num - 1)

  let rec row_helper board r c =
    match board with
    | [] -> Empty
    | h :: t -> if r = 0 then col_helper h c else row_helper t (r - 1) c

  let piece_at_square board c r = row_helper board r c

  let print_matcher str = function
    | King (Black, _) -> str ^ "♔"
    | Queen (Black, _) -> str ^ "♕"
    | Rook (Black, _) -> str ^ "♖"
    | Bishop (Black, _) -> str ^ "♗"
    | Knight (Black, _) -> str ^ "♘"
    | Pawn (Black, _) -> str ^ "♙"
    | King (White, _) -> str ^ "♚"
    | Queen (White, _) -> str ^ "♛"
    | Rook (White, _) -> str ^ "♜"
    | Bishop (White, _) -> str ^ "♝"
    | Knight (White, _) -> str ^ "♞"
    | Pawn (White, _) -> str ^ "♟"
    | _ -> str ^ "_"

  let col_helper row = List.fold_left print_matcher "" row ^ "\n"

  let rec row_helper board acc =
    match board with
    | [] -> print_endline acc
    | h :: t -> row_helper t (acc ^ col_helper h)

  let display_board board = row_helper board ""

  let rec access_square_col replacer row curr_c target_c acc =
    match row with
    | [] -> List.rev acc
    | h :: t ->
        access_square_col replacer t (curr_c + 1) target_c
          (if curr_c = target_c then replacer :: acc else h :: acc)

  let rec access_square_row replacer board curr_r acc target_r target_c =
    match board with
    | [] -> List.rev acc
    | h :: t ->
        access_square_row replacer t (curr_r + 1)
          (if curr_r = target_r then
           access_square_col replacer h 0 target_c [] :: acc
          else h :: acc)
          target_r target_c

  let access_square replacer board target_r target_c =
    access_square_row replacer board 0 [] target_r target_c
end

let convert_to_col input = Char.code input - Char.code 'a'
let convert_to_row input = 7 - (Char.code input - Char.code '1')

let parse_move m =
  let caseless = m |> String.lowercase_ascii in
  {
    start = (convert_to_col caseless.[0], convert_to_row caseless.[1]);
    dest = (convert_to_col caseless.[3], convert_to_row caseless.[4]);
  }

let confines_checker (c, r) = c > -1 && c < 8 && r > -1 && r < 8

let rec move_helper p board change_c change_r curr_c curr_r end_c end_r =
  let curr_square = RegularChessBoard.piece_at_square board curr_c curr_r in
  match curr_square with
  | Empty ->
      if curr_c = end_c && curr_r = end_r then true
      else
        move_helper p board change_c change_r (curr_c + change_c)
          (curr_r + change_r) end_c end_r
  | _ ->
      if
        Basic.color curr_square <> Basic.color p
        && curr_c = end_c && curr_r = end_r
      then true
      else false

let the_move_checker piece board start_c start_r dest_c dest_r =
  match piece with
  | Knight _ -> true
  | _ ->
      if start_c = dest_c then
        if start_r < dest_r then
          move_helper piece board 0 1 start_c (start_r + 1) dest_c dest_r
        else move_helper piece board 0 (-1) start_c (start_r - 1) dest_c dest_r
      else if start_r = dest_r then
        if start_c < dest_c then
          move_helper piece board 1 0 (start_c + 1) start_r dest_c dest_r
        else move_helper piece board (-1) 0 (start_c - 1) start_r dest_c dest_r
      else if dest_c < start_c && dest_r > start_r then
        move_helper piece board (-1) 1 (start_c - 1) (start_r + 1) dest_c dest_r
      else if dest_c < start_c && dest_r < start_r then
        move_helper piece board (-1) (-1) (start_c - 1) (start_r - 1) dest_c
          dest_r
      else if dest_c > start_c && dest_r < start_r then
        move_helper piece board 1 (-1) (start_c + 1) (start_r - 1) dest_c dest_r
      else move_helper piece board 1 1 (start_c + 1) (start_r + 1) dest_c dest_r

let check_legal board piece (a, b) (c, d) =
  if not (confines_checker (a, b) && confines_checker (c, d)) then false
  else if (a, b) = (c, d) then false
  else
    let target = RegularChessBoard.piece_at_square board c d in
    if Basic.color target = Basic.color piece then false
    else if
      match piece with
      | King _ -> King.movement piece (a, b) (c, d)
      | Queen _ -> Queen.movement piece (a, b) (c, d)
      | Rook _ -> Rook.movement piece (a, b) (c, d)
      | Bishop _ -> Bishop.movement piece (a, b) (c, d)
      | Knight _ -> Knight.movement piece (a, b) (c, d)
      | Pawn _ -> Pawn.movement piece (a, b) (c, d)
      | Empty -> false
    then the_move_checker piece board a b c d
    else false

type a_move =
  | Illegal
  | Legal of chess_piece * coord * coord

let valid_move m board =
  let move = parse_move m in
  let a, b = move.start in
  let c, d = move.dest in
  let piece = RegularChessBoard.piece_at_square board a b in
  if check_legal board piece move.start move.dest then
    Legal (piece, (a, b), (c, d))
  else Illegal

let remove_piece c r board = RegularChessBoard.access_square Empty board r c

let add_piece piece dc dr board_with_removed =
  RegularChessBoard.access_square piece board_with_removed dr dc

let move_the_piece piece sc sr dc dr board =
  let board_with_removed = remove_piece sc sr board in
  add_piece piece dc dr board_with_removed

let rec play_the_game board =
  RegularChessBoard.display_board board;
  print_endline "Enter move in format <startpos> <endpos> \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> Stdlib.exit 0
  | move -> (
      match valid_move move board with
      | Illegal ->
          print_endline "\nIllegal\n";
          play_the_game board
      | Legal (p, (sc, sr), (dc, dr)) ->
          print_endline "\nLegal\n";
          play_the_game (move_the_piece p sc sr dc dr board))

let play h =
  if h = 7 then print_endline "You have started a new game of chess\n";
  play_the_game RegularChessBoard.create

let () = play 7
