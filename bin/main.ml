let nums = [ 1.; 3.; 4.; 7. ]

let print_list l =
  print_endline @@ Printf.sprintf "[%s]" @@ String.concat "\n" l

let () = print_list @@ Solver.solve nums 3.
