module Board = struct
  type player = PlayerX | PlayerO | NoPlayer [@@deriving show]

  let int_of_player = function PlayerX -> -1 | PlayerO -> 1 | NoPlayer -> 0
  let player_of_int = function -1 -> PlayerX | 0 -> PlayerO | _ -> NoPlayer

  let opposite_player = function
    | PlayerX -> PlayerO
    | PlayerO -> PlayerX
    | _ -> NoPlayer

  type board = { player_to_move : player; state : player list; row_size : int }
  [@@deriving show]

  let new_ row_size =
    let state = List.init (row_size * row_size) (function _ -> NoPlayer) in
    { player_to_move = PlayerX; state; row_size }

  let make_move move board =
    let move_range = board.row_size * board.row_size in
    let _ =
      (* TODO: is there a better way to do this *)
      if move >= move_range || move < 0 then
        failwith
        @@ Printf.sprintf "move out of range [0, %d): %d" move_range move
      else ()
    in

    let state =
      List.mapi
        (fun i v -> if i == move then board.player_to_move else v)
        board.state
    in
    let player_to_move = opposite_player board.player_to_move in
    { board with state; player_to_move }

  let compute_winning_lines _board =
    [
      [ 0; 1; 2 ];
      [ 3; 4; 5 ];
      [ 6; 7; 8 ];
      [ 0; 3; 6 ];
      [ 1; 4; 7 ];
      [ 2; 5; 8 ];
      [ 0; 4; 8 ];
      [ 2; 4; 6 ];
    ]

  let rec check_line_aux board score line =
    match line with
    | [] -> (
        let result = score / board.row_size in
        match abs result with 1 -> player_of_int result | _ -> NoPlayer)
    | hd :: tl ->
        let board_value = List.nth board.state hd in
        let score = score + int_of_player board_value in
        check_line_aux board score tl

  let check_line board line = check_line_aux board 0 line

  let rec check_lines_aux board lines results =
    match lines with
    | [] -> results
    | hd :: tl -> check_lines_aux board tl (check_line board hd :: results)

  let check_lines board lines = check_lines_aux board lines []

  let is_game_over board =
    let results =
      compute_winning_lines board.row_size
      |> check_lines board
      |> List.filter (fun r -> r != NoPlayer)
    in
    List.nth_opt results 0
end

let row_size = 3

let () =
  let b = Board.new_ row_size in
  print_newline ();
  (* let b = Board.make_move 0 b in *)
  let b =
    b |> Board.make_move 0 |> Board.make_move 3 |> Board.make_move 1
    |> Board.make_move 4 |> Board.make_move 2
  in
  print_newline ();
  print_string @@ Board.show_board b;
  print_newline ();
  let result = Board.is_game_over b in
  print_string
    (match result with
    | None -> "game in progress"
    | Some v -> Board.show_player v);
  print_newline ()
