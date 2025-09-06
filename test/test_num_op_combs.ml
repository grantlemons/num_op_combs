open Alcotest

let test_possible_outputs () =
  check
    (list (float 0.01))
    "same lists" [ -1.; 0.5; 1.; 2.; 3. ]
    (Solver.possible_outputs [ 1.; 2. ])

let () =
  run "Known Inputs"
    [
      ( "possible_outputs",
        [ test_case "list_matching" `Slow test_possible_outputs ] );
    ]
