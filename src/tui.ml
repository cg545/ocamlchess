open Chess
open General
open RuleSet
open Board
open Game
open General
open Str
open ColorScheme

(** ANSI code to reset style*)
let reset_style = "\x1B[0m"

(** Sets cursor at position x y and cleares everything below it*)
let set_and_clear x y =
  ANSITerminal.set_cursor x y;
  ANSITerminal.erase ANSITerminal.Below

(** Sets cursor at top-left corner of screen and clears entire screen*)
let all_clear () = set_and_clear 1 1

(** All the coords in a rank*)
let coords_in_rank rank = 1 -- 8 |> List.map (fun file -> (rank, file))

(** 90 degree rotation of all_coords in Board because it looks nicer for
    printing*)
let pretty_coords = 1 -- 8 |> List.map coords_in_rank

(** Prints [str] with the ANSI escape codes in [styles] appended to the front*)
let ansi_print styles str =
  print_string
    (List.fold_left (fun acc elem -> elem ^ acc) str styles ^ reset_style)

(**[num_to_file num] is the alphabetic representation of num as a file*)
let num_to_file num = String.make 1 (Char.chr (64 + num))

(** [print_tile coord game selected movable color_scheme] prints the [coord] in
    [game] per the rules of [color_scheme]. [selected] represents the coord
    selected by the player to view. Is the [None] variant if no such coord
    selected. [movable] represents all coords that can be moved to from the
    selected coord*)
let print_tile coord game selected movable color_scheme =
  let file, rank = coord in
  if rank = 1 then
    ansi_print [ file_rank_label file color_scheme ] (num_to_file file ^ " ");
  let background = background color_scheme coord selected movable in
  (match Game.inspect game coord with
  | Game.Empty -> ansi_print [ background ] "  "
  | Game.Occupied (piece, color) ->
      ansi_print [ background ]
        (ColorScheme.piece color_scheme (piece, color) coord selected movable));
  if rank = 8 then print_string "\n" else ()

(** Prints the game board. If optional_coord is [Some coord] then [coord] will
    be printed with a special background color and everywhere it can move will
    be as well*)
let pretty_print game optional_coord color_scheme =
  (* Dummy let statement prevents compiler warning (because the expression is
     unit list list instead of unit)*)
  let movable =
    match optional_coord with
    | Some coord -> Game.legal_moves game coord
    | None -> []
  in
  print_string " ";
  let _ =
    1 -- 8
    |> List.map (fun num ->
           ansi_print
             [ file_rank_label num color_scheme ]
             (" " ^ string_of_int num))
  in
  print_endline "";
  let _ =
    pretty_coords
    |> List.map (fun rows ->
           List.map
             (fun coord ->
               print_tile coord game optional_coord movable color_scheme)
             rows)
  in
  ()

(** Accepts terminal input for a number. Will not return until a proper numeric
    value is entered.*)
let rec parse_number () =
  match read_line () with
  | num -> (
      try int_of_string num
      with Failure _ ->
        print_endline "You must enter a numeric value";
        parse_number ())

(** Keeps prompting until the parsed number is greater than or equal to lower
    and less than or equal to upper*)
let rec parse_number_in_bounds lower upper =
  let parsed = parse_number () in
  if parsed > upper || parsed < lower then (
    print_endline
      ("You must enter a number in the inclusive range " ^ string_of_int lower
     ^ "-" ^ string_of_int upper);
    parse_number_in_bounds lower upper)
  else parsed

(** Creates a prompt for user input for an association list whose keys are the
    names of the elements of the list*)
let association_list_prompt prompt_text (lst : (string * 'a) list) : string * 'a
    =
  let indices_to_pairs = List.mapi (fun i pair -> (i, pair)) lst in
  let output_string =
    List.fold_left
      (fun acc pair ->
        let i, (name, _) = pair in
        acc ^ string_of_int i ^ ": " ^ name ^ "\n")
      "" indices_to_pairs
  in
  print_string (prompt_text ^ output_string);
  let num = parse_number_in_bounds 0 (List.length lst - 1) in
  let selected_name, selected = List.nth lst num in
  (selected_name, selected)

(** Converts a string into a coord. Is [None] if the string cannot be converted.
    Case-insensitive.*)
let coord_of_string str : coord option =
  let str' = String.uppercase_ascii str in
  if Str.string_match (Str.regexp "[A-H][1-8]") str' 0 && String.length str' = 2
  then Some (Char.code str'.[0] - 64, Char.code str'.[1] - 48)
  else None

(** Accepts input of the form <A1 A2> or <A1> from the player. If the form <A1
    A2> is used, returns a tuple of the first coordinate and [Some] of the
    second coordinate. If <A1> is used, returns tuple of the coordinate and
    [None]. Will not return until given good input. Will not accept illegal
    moves per [game]*)
let rec parse_movement_or_selection game color_scheme : coord * coord option =
  match List.map coord_of_string (String.split_on_char ' ' (read_line ())) with
  | [ Some coord ] -> (coord, None)
  | [ Some coord1; Some coord2 ] -> (
      match Game.move game coord1 coord2 with
      | Illegal _ ->
          ansi_print [ neutral_text color_scheme ] "Illegal move\n";
          parse_movement_or_selection game color_scheme
      | Legal _ -> (coord1, Some coord2))
  | _ ->
      ansi_print [ neutral_text color_scheme ] "Move <A1 A2> or select <A1>\n";
      parse_movement_or_selection game color_scheme

let promotions =
  [ ("Queen", Queen); ("Rook", Rook); ("Bishop", Bishop); ("Knight", Knight) ]

(** Creates a prompt for movement or sleection for a turn. Will output the final
    game state without prompting for more information when the game concludes.

    [game_mode] represents the name of the game mode. [game] represents the
    state of the game that the player will be asked to act upon.
    [optional_coord] represents the selected tile, which is printed differently,
    as are all legal moves from that tile. [None] if no tile selected.
    [color_scheme] defines the colors that the game will be printed in, as well
    as other stylistic elements*)
let rec prompt_turn game_mode game (optional_coord : coord option) color_scheme
    : unit =
  all_clear ();
  match Game.winner game with
  | Winner color ->
      if color = Black then
        ansi_print
          [ black_text color_scheme ]
          ("Black has won this round of " ^ game_mode ^ "\n\n")
      else
        ansi_print
          [ white_text color_scheme ]
          ("White has won this round of " ^ game_mode ^ "\n\n");
      pretty_print game None color_scheme
  | Stalemate ->
      ansi_print
        [ neutral_text color_scheme ]
        ("This round of " ^ game_mode ^ " ends in stalemate\n\n");
      pretty_print game None color_scheme
  | Ongoing -> (
      match promotion_available game with
      | Some coord ->
          ansi_print
            (if turn game = White then [ black_text color_scheme ]
            else [ white_text color_scheme ])
            (game_mode ^ " - "
            ^
            if turn game = White then "Black must select a promotion\n\n"
            else "White must select a promotion\n\n");
          pretty_print game (Some coord) color_scheme;
          let _, promotion =
            association_list_prompt "Select the promotion:\n" promotions
          in
          prompt_turn game_mode
            (match promote game coord promotion with
            | Legal game' -> game'
            | Illegal _ ->
                failwith "Should never prompt for promotion illegally")
            None color_scheme
      | None -> (
          ansi_print
            (if turn game = White then [ white_text color_scheme ]
            else [ black_text color_scheme ])
            (game_mode ^ " - "
            ^
            if turn game = White then "White's turn\n\n" else "Black's turn\n\n"
            );
          pretty_print game optional_coord color_scheme;
          ansi_print
            [ neutral_text color_scheme ]
            "\nMove <A1 A2> or select <A1>\n";
          match parse_movement_or_selection game color_scheme with
          | coord, None -> prompt_turn game_mode game (Some coord) color_scheme
          | coord1, Some coord2 -> (
              match Game.move game coord1 coord2 with
              | Legal game -> prompt_turn game_mode game None color_scheme
              | Illegal game -> failwith "bad input accepted")))

(** Handles initial game setup. Prints decoration, prompts for ruleset and color
    scheme, and then prompts for turns until the game is concluded.*)
let tui () =
  all_clear ();
  print_string
    {|     ___           ___           ___           ___           ___     
    /\  \         /\__\         /\  \         /\  \         /\  \    
   /::\  \       /:/  /        /::\  \       /::\  \       /::\  \   
  /:/\:\  \     /:/__/        /:/\:\  \     /:/\ \  \     /:/\ \  \  
 /:/  \:\  \   /::\  \ ___   /::\~\:\  \   _\:\~\ \  \   _\:\~\ \  \ 
/:/__/ \:\__\ /:/\:\  /\__\ /:/\:\ \:\__\ /\ \:\ \ \__\ /\ \:\ \ \__\
\:\  \  \/__/ \/__\:\/:/  / \:\~\:\ \/__/ \:\ \:\ \/__/ \:\ \:\ \/__/
 \:\  \            \::/  /   \:\ \:\__\    \:\ \:\__\    \:\ \:\__\  
  \:\  \           /:/  /     \:\ \/__/     \:\/:/  /     \:\/:/  /  
   \:\__\         /:/  /       \:\__\        \::/  /       \::/  /   
    \/__/         \/__/         \/__/         \/__/         \/__/    

|};
  let game_mode, ruleset =
    association_list_prompt "Select game mode:\n" sets_of_rules
  in
  let _, color_scheme =
    association_list_prompt "Select color scheme:\n" color_schemes
  in
  prompt_turn game_mode (Game.make ruleset) None color_scheme

(** Executes the program *)
let () = tui ()
