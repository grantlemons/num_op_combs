let nums = [ 1.; 3.; 4.; 7. ]

let print l t =
  let format_list = Printf.sprintf "[%s]" (String.concat "\n" l) in
  let format_time = Printf.sprintf "\n%f" t in
  print_endline (format_list ^ format_time)

let uncurry f (x, y) = f x y

let time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  (res, stop -. start)

let () = uncurry print @@ time @@ fun () -> Solver.solve nums 3.
