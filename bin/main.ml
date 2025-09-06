let nums = [ 1.; 3.; 4.; 7. ]

let () =
  print_char '[';
  List.iter print_endline @@ Solver.solve nums 3.;
  print_endline "]"
