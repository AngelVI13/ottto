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

    let compute_winning_lines board =
        [[0;1;2];[3;4;5];[6;7;8];[0;3;6];[1;4;7];[2;5;8]]

    let is_game_over board = 
        let lines = compute_winning_lines board.row_size in
        None

let row_size = 3

let () =
  let b = Board.new_ row_size in
  print_newline ();
  let b = Board.make_move 0 b in
  print_int @@ List.length b.state;
  print_newline ();
  print_string @@ Board.show_board b
