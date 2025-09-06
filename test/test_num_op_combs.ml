open Alcotest

let test_possible_outputs () =
  check (list string) "expected output" [] (Solver.solve [ 1.; 3.; 5.; 7. ] 3.)

let () =
  run "Known Inputs"
    [
      ( "possible_outputs",
        [ test_case "list_matching" `Slow test_possible_outputs ] );
    ]
