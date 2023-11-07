module Board = struct
  type player = PlayerX | PlayerO | NoPlayer [@@deriving show]

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
    let player_to_move =
      match board.player_to_move with
      | PlayerX -> PlayerO
      | PlayerO -> PlayerX
      | NoPlayer -> failwith "unexpected player to move: NoPlayer"
    in
    { board with state; player_to_move }

  let compute_winning_lines _board =
    [[0;1;2];[3;4;5];[6;7;8];[0;3;6];[1;4;7];[2;5;8];[0;4;8];[2;4;6]]

  let rec check_line_aux board score line =
    match line with 
    | [] -> ( match abs(score)/board.row_size with | -1 -> PlayerX | 1 -> PlayerO | _ -> NoPlayer)
    | hd :: tl -> check_line_aux board (match List.nth board.state hd with | PlayerX -> score -1 | PlayerO -> +1 | _ -> score) tl

 let check_line board line =
     check_line_aux board 0 line

 let rec check_lines_aux board lines results =
     match lines with
     | [] -> results
     | hd::tl -> check_lines_aux board tl ((check_line board hd)::results)

 let check_lines board lines =
     check_lines_aux board lines []


  let is_game_over board = 
      let results = compute_winning_lines board.row_size |> check_lines board |> List.filter (fun r -> r != NoPlayer) in
      List.nth_opt results 0

  let row_size = 3

  let () =
    let b = Board.new_ row_size in
    print_newline ();
    let b = Board.make_move 0 b in
    print_int @@ List.length b.state;
    print_newline ();
    print_string @@ Board.show_board b
