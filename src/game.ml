open General
open RuleSet
open Board

type t = {
  board : Board.t;
  rule_set : RuleSet.t;
}

type tile_to_draw =
  | Empty
  | Occupied of piece_type * color

let make rule_set = { board = initial_board rule_set; rule_set }
let turn game = if Board.turn_counter game.board mod 2 = 0 then White else Black
let giveBoard game = game.board

let inspect game coord =
  match Board.inspect game.board coord with
  | None -> Empty
  | Some s -> begin
      match s with
      | Empty -> Empty
      | Occupied p -> Occupied (p.piece_type, p.color)
    end

let legal_moves game coord =
  match inspect game coord with
  | Occupied (_, color) when color = turn game ->
      RuleSet.moves game.rule_set game.board coord
  | _ -> []

let move game start dest =
  if List.mem dest (legal_moves game start) then
    let moved_piece = Board.move_and_capture game.board start dest dest in
    match moved_piece with
    | Legal x -> Legal { board = x; rule_set = game.rule_set }
    | Illegal x -> Illegal game
  else Illegal game

let winner game = RuleSet.winner game.rule_set game.board

let promotion_available game =
  match last_move game.board with
  | None -> None
  | Some ((file, rank), square) -> (
      match square with
      | Empty -> None
      | Occupied { piece_type; _ } -> (
          match piece_type with
          | Pawn when rank = 1 || rank = 8 -> Some (file, rank)
          | _ -> None))

let promote game coord promote_to =
  let moved_promote = Board.promote game.board coord promote_to in
  match moved_promote with
  | Legal x -> Legal { game with board = x }
  | Illegal x -> Illegal game
