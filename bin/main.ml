module Board = struct
  type player = PlayerX | (* PlayerO | *) NoPlayer
  type board = { player_to_move : player; state : player list }

  let new_ row_size =
    let state = List.init (row_size * row_size) (function _ -> NoPlayer) in
    { player_to_move = PlayerX; state }
end

let row_size = 3

let () =
  let b = Board.new_ row_size in
  let x = match b.player_to_move with PlayerX -> -1 | NoPlayer -> 0 in
  print_int x;
  print_newline ();
  (* TODO: how to print PlayerX & others, also what values do they carry ? *)
  print_int @@ List.length b.state;
  print_newline ()
