let nums = [ 1.; 3.; 4.; 7. ]

let print_list l =
  print_endline @@ Printf.sprintf "[%s]" @@ String.concat "\n" l

let time_call f =
  let start = Unix.gettimeofday () in
  let _ = f () in
  let stop = Unix.gettimeofday () in
  print_endline @@ Printf.sprintf "%f s" (stop -. start)

let () = time_call @@ fun () -> print_list @@ Solver.solve nums 3.
