open General

type chesspiece = {
  piece_type : piece_type;
  color : color;
  has_moved : bool;
}

type square =
  | Empty
  | Occupied of chesspiece

type t = {
  turn_counter : int;
  board : (coord * square) list;
  last_move : (coord * square) option;
  pawn_move2 : bool;
}

let make asoc_lst =
  { turn_counter = 0; board = asoc_lst; last_move = None; pawn_move2 = false }

let last_move b = b.last_move
let did_pawn_move2 b = b.pawn_move2

let inspect b coord =
  try Some (List.assoc coord b.board) with Not_found -> None

let turn_counter b = b.turn_counter

(*Check if any coordinates exceed the 8x8 bounds, must return true*)
let checkBounds (location : coord) =
  match location with
  | file, rank -> file >= 1 && file <= 8 && rank >= 1 && rank <= 8

(*Check if start contains an empty square*)
let assertEmpty b coord =
  match inspect b coord with
  | None -> false
  | Some x -> x = Empty

(*Checks if start/dest/capture are within 8x8 bounds, ensures start isn't empty,
  ensures destination is empty*)
let rec replacer target_k new_v board acc =
  match board with
  | [] -> (List.rev acc, (target_k, new_v))
  | (k, v) :: t ->
      replacer target_k new_v t
        ((if k = target_k then (k, new_v) else (k, v)) :: acc)

let handle_en_passant start dest the_pawn_opt board counter =
  match the_pawn_opt with
  | Occupied the_pawn ->
      let remove_pawn, _ = replacer start Empty board [] in
      let place_pawn_behind, _ = replacer dest the_pawn_opt remove_pawn [] in
      let remove_opp_pawn, last_m =
        if the_pawn.color = White then
          replacer (fst dest, snd dest - 1) Empty place_pawn_behind []
        else replacer (fst dest, snd dest + 1) Empty place_pawn_behind []
      in

      Legal
        {
          turn_counter = counter + 1;
          board = remove_opp_pawn;
          last_move = Some last_m;
          pawn_move2 = false;
        }
  | _ ->
      Illegal
        { turn_counter = counter; board; last_move = None; pawn_move2 = false }

let rec queenside_castle start dest the_king board =
  match List.assoc (1, snd start) board with
  | Occupied r ->
      let the_rook = Occupied { r with has_moved = true } in
      let remove_king, _ = replacer start Empty board [] in
      let king_move_right, _ = replacer dest the_king remove_king [] in
      let remove_rook, _ = replacer (1, snd dest) Empty king_move_right [] in
      let add_rook_back, last_m =
        replacer (4, snd dest) the_rook remove_rook []
      in
      (add_rook_back, Some last_m)
  | _ -> (board, None)

let rec kingside_castle start dest the_king board =
  match List.assoc (8, snd start) board with
  | Occupied r ->
      let the_rook = Occupied { r with has_moved = true } in
      let remove_king, _ = replacer start Empty board [] in
      let king_move_right, _ = replacer dest the_king remove_king [] in
      let remove_rook, _ = replacer (8, snd dest) Empty king_move_right [] in
      let add_rook_back, last_m =
        replacer (6, snd dest) the_rook remove_rook []
      in
      (add_rook_back, Some last_m)
  | _ -> (board, None)

let handle_castle diff_in_file start dest the_king board counter =
  let after_the_castle, last_m =
    if diff_in_file > 0 then kingside_castle start dest the_king board
    else queenside_castle start dest the_king board
  in
  Legal
    {
      turn_counter = counter + 1;
      board = after_the_castle;
      last_move = last_m;
      pawn_move2 = false;
    }

let move_and_capture board start dest capture =
  if not (checkBounds start && checkBounds dest && checkBounds capture) then
    Illegal board
  else
    let capture_removed, _ = replacer capture Empty board.board [] in
    if not (assertEmpty { board with board = capture_removed } dest) then
      Illegal board
    else
      let start_piece = inspect board start in
      match (start_piece, inspect board dest) with
      | None, _ -> Illegal board
      | _, None -> Illegal board
      | Some x, y -> begin
          match x with
          | Empty -> Illegal board
          | Occupied p ->
              let updated_x = Occupied { p with has_moved = true } in
              let is_en_pass =
                p.piece_type = Pawn && y = Some Empty
                && fst start <> fst dest
                && snd start <> snd dest
              in
              if is_en_pass then
                handle_en_passant start dest updated_x capture_removed
                  board.turn_counter
              else
                let diff_in_file = fst dest - fst start in
                let is_castle = p.piece_type = King && abs diff_in_file = 2 in
                if is_castle then
                  handle_castle diff_in_file start dest updated_x
                    capture_removed board.turn_counter
                else
                  let piece_picked_up, _ =
                    replacer start Empty capture_removed []
                  in
                  let piece_placed_down, last_m =
                    replacer dest updated_x piece_picked_up []
                  in
                  Legal
                    {
                      turn_counter = board.turn_counter + 1;
                      board = piece_placed_down;
                      last_move = Some last_m;
                      pawn_move2 =
                        p.piece_type = Pawn && abs (snd start - snd dest) = 2;
                    }
        end

let promote board coord promote_to =
  match List.assoc coord board.board with
  | Empty -> Illegal board
  | Occupied old_piece ->
      Legal
        {
          board with
          board =
            (coord, Occupied { old_piece with piece_type = promote_to })
            :: List.remove_assoc coord board.board;
          last_move =
            Some (coord, Occupied { old_piece with piece_type = promote_to });
        }

let string_to_square str =
  if str = "_" then Empty
  else
    let piece_type, color =
      match str with
      | "k" -> (King, Black)
      | "K" -> (King, White)
      | "q" -> (Queen, Black)
      | "Q" -> (Queen, White)
      | "r" -> (Rook, Black)
      | "R" -> (Rook, White)
      | "b" -> (Bishop, Black)
      | "B" -> (Bishop, White)
      | "n" -> (Knight, Black)
      | "N" -> (Knight, White)
      | "p" -> (Pawn, Black)
      | "P" -> (Pawn, White)
      | _ as illegal -> failwith ("Illegal character: " ^ illegal)
    in
    Occupied { piece_type; color; has_moved = false }

(** Turns a piece into a string representation where pieces use their standard
    abbreviations. Black pieces are lowercase while white piece are uppercase*)
let piece_to_string piece =
  let piece_type, color = (piece.piece_type, piece.color) in
  match (piece_type, color) with
  | King, Black -> "k"
  | King, White -> "K"
  | Queen, Black -> "q"
  | Queen, White -> "Q"
  | Rook, Black -> "r"
  | Rook, White -> "R"
  | Bishop, Black -> "b"
  | Bishop, White -> "B"
  | Knight, Black -> "n"
  | Knight, White -> "N"
  | Pawn, Black -> "p"
  | Pawn, White -> "P"

let square_to_string square =
  match square with
  | Occupied piece -> piece_to_string piece
  | Empty -> "_"

let coords_in_rank rank = List.map (fun file -> (file, rank)) (1 -- 8)
let all_coords = List.map (fun rank -> coords_in_rank rank) (8 -- 1)

let board_to_string board =
  all_coords
  |> List.map (List.map (fun coord -> inspect board coord))
  |> List.map
       (List.map (fun square ->
            match square with
            | None ->
                failwith "Attempting to print board that's missing some tiles"
            | Some sq -> square_to_string sq))
  |> List.fold_left
       (fun acc elem ->
         acc ^ "\n" ^ List.fold_left (fun acc2 elem2 -> acc2 ^ elem2) "" elem)
       ""

let string_to_board str =
  str |> String.trim |> String.split_on_char '\n' |> List.map expand_string
  |> List.mapi (fun rank lst ->
         List.mapi
           (fun file str -> ((file + 1, 8 - rank), string_to_square str))
           lst)
  |> List.flatten |> make