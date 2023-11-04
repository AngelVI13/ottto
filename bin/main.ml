module Board = struct
  type player = PlayerX | PlayerO | NoPlayer
  type board = { player_to_move : player; state : player list; row_size : int }

  let new_ row_size =
    let state = List.init (row_size * row_size) (function _ -> NoPlayer) in
    { player_to_move = PlayerX; state; row_size }

  (* TODO: check if move in range *)
  let make_move move board =
    let state =
      List.mapi
        (fun i v -> if i == move then board.player_to_move else v)
        board.state
    in
    let player_to_move =
      match board.player_to_move with
      | PlayerX -> PlayerO
      | PlayerO -> PlayerX
      | NoPlayer -> PlayerX
    in
    { board with state; player_to_move }
end

let row_size = 3

let () =
  let b = Board.new_ row_size in
  print_newline ();
  (* TODO: how to print PlayerX & others, also what values do they carry ? *)
  print_int @@ List.length b.state;
  print_newline ();
  print_int b.row_size;
  print_newline ()
