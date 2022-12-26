open General
open Yojson.Basic.Util

type t = Yojson.Basic.t

let is_dark (file, rank) = (file + rank) mod 2 = 0
let ansi_wrap str = "\x1B[" ^ str ^ "m"
let name color_scheme = color_scheme |> member "name"

let extract_required_string name color_scheme =
  color_scheme |> member name |> to_string |> ansi_wrap

let normal_background_light color_scheme =
  extract_required_string "normal_background_light" color_scheme

let normal_background_dark color_scheme =
  extract_required_string "normal_background_dark" color_scheme

let normal_background coord color_scheme =
  if is_dark coord then normal_background_dark color_scheme
  else normal_background_light color_scheme

let extract_string_default_dark coord name color_scheme =
  let dark = color_scheme |> member (name ^ "_dark") |> to_string in
  (match color_scheme |> member (name ^ "_light") with
  | `Null -> dark
  | `String light -> if is_dark coord then dark else light
  | _ -> failwith "Poorly formatted JSON")
  |> ansi_wrap

let highlighted_background coord color_scheme =
  extract_string_default_dark coord "highlighted_background" color_scheme

let selected_background coord color_scheme =
  extract_string_default_dark coord "selected_background" color_scheme

let extract_bool name default color_scheme =
  match color_scheme |> member name with
  | `Null -> default
  | `Bool boolean -> boolean
  | _ -> default

let white_filled color_scheme = extract_bool "white_filled" true color_scheme
let black_filled color_scheme = extract_bool "black_filled" true color_scheme

let selected_swap_fill color_scheme =
  extract_bool "selected_swap_fill" false color_scheme

let highlighted_swap_fill color_scheme =
  extract_bool "highlighted_swap_fill" false color_scheme

let white_piece_style color_scheme =
  extract_required_string "white_piece_style" color_scheme

let black_piece_style color_scheme =
  extract_required_string "black_piece_style" color_scheme

let extract_optional_string name default color_scheme =
  match color_scheme |> member name with
  | `String style -> ansi_wrap style
  | _ -> default

let black_text color_scheme =
  extract_optional_string "black_text_style"
    (black_piece_style color_scheme ^ normal_background_light color_scheme)
    color_scheme

let white_text color_scheme =
  extract_optional_string "white_text_style"
    (white_piece_style color_scheme ^ normal_background_dark color_scheme)
    color_scheme

let neutral_text color_scheme =
  extract_optional_string "neutral_text_style" "" color_scheme

let selected_style color_scheme =
  extract_optional_string "selected_style" "" color_scheme

let highlighted_style color_scheme =
  extract_optional_string "highlighted_style" "" color_scheme

let file_rank_label num color_scheme =
  let is_dark = num mod 2 <> 0 in
  (match
     ( color_scheme |> member "file_rank_style_dark",
       color_scheme |> member "file_rank_style_light" )
   with
  | `String dark, `String light -> if is_dark then dark else light
  | `String dark, _ -> dark
  | _, _ -> "")
  |> ansi_wrap

let background color_scheme this_coord selected_coord highlighted_coords =
  match (this_coord, selected_coord) with
  | this_coord, Some selected_coord when this_coord = selected_coord ->
      selected_background this_coord color_scheme
  | this_coord, _ when List.mem this_coord highlighted_coords ->
      highlighted_background this_coord color_scheme
  | this_coord, _ -> normal_background this_coord color_scheme

let filled_piece piece_type =
  match piece_type with
  | King -> "♚"
  | Queen -> "♛"
  | Rook -> "♜"
  | Bishop -> "♝"
  | Knight -> "♞"
  | Pawn -> "♟"

let unfilled_piece piece_type =
  match piece_type with
  | King -> "♔"
  | Queen -> "♕"
  | Rook -> "♖"
  | Bishop -> "♗"
  | Knight -> "♘"
  | Pawn -> "♙"

let spacing piece_string color =
  if color = White then " " ^ piece_string else piece_string ^ " "

let piece color_scheme (piece_type, color) this_coord selected_coord
    highlighted_coords =
  let is_selected =
    match selected_coord with
    | Some selected when selected = this_coord -> true
    | _ -> false
  in
  let is_highlighted = List.mem this_coord highlighted_coords in
  let swap_fill =
    (is_selected && selected_swap_fill color_scheme)
    || (is_highlighted && highlighted_swap_fill color_scheme)
  in
  let filled =
    match color with
    | White ->
        if swap_fill then not (white_filled color_scheme)
        else white_filled color_scheme
    | Black ->
        if swap_fill then not (black_filled color_scheme)
        else black_filled color_scheme
  in
  let piece_string =
    spacing ((if filled then filled_piece else unfilled_piece) piece_type) color
  in
  let style =
    match (color, is_selected, is_highlighted) with
    | White, true, _ ->
        selected_style color_scheme ^ white_piece_style color_scheme
    | White, _, true ->
        highlighted_style color_scheme ^ white_piece_style color_scheme
    | White, _, _ -> white_piece_style color_scheme
    | Black, true, _ ->
        selected_style color_scheme ^ black_piece_style color_scheme
    | Black, _, true ->
        highlighted_style color_scheme ^ black_piece_style color_scheme
    | Black, _, _ -> black_piece_style color_scheme
  in
  style ^ piece_string

let color_schemes =
  Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "color_schemes.json")
  |> to_assoc
  |> List.map (fun pair ->
         let name, json = pair in
         ( normal_background_dark json
           ^ white_piece_style json ^ name ^ "\x1B[0m",
           json ))
