type op = Add | Sub | Mul | Div | RSub | RDiv

let rec apply = function
  | Add -> ( +. )
  | Sub -> ( -. )
  | Mul -> ( *. )
  | Div -> ( /. )
  | RSub -> fun a b -> apply Sub b a
  | RDiv -> fun a b -> apply Div b a

let inverse = function
  | Add -> Sub
  | Sub -> Add
  | Mul -> Div
  | Div -> Mul
  | RSub -> Add
  | RDiv -> Mul

let op_to_string x y op =
  let ( >>= ) = Option.bind in
  x >>= fun a ->
  y >>= fun b ->
  Some
    (match op with
    | Add -> Printf.sprintf "(%s %s %s)" a "+" b
    | Sub -> Printf.sprintf "(%s %s %s)" a "-" b
    | Mul -> Printf.sprintf "(%s %s %s)" a "*" b
    | Div -> Printf.sprintf "(%s %s %s)" a "/" b
    | RSub -> Printf.sprintf "(%s %s %s)" b "-" a
    | RDiv -> Printf.sprintf "(%s %s %s)" b "/" a)

let nums = [ 1.; 3.; 4.; 7. ]

(* RDiv and RSub are Div and Sub with swapped inputs *)
let ops = [ Add; Sub; Mul; Div; RSub; RDiv ]

(* Returns all possible combinations of operators and a list of two numbers *)
let possible_outputs =
  let module FloatSet = Set.Make (Float) in
  function
  | [ a; b ] ->
      List.map (fun op -> apply op a b) ops
      |> FloatSet.of_list |> FloatSet.to_list
  | _ -> failwith "List not of two elements"

(* Assumes len(nums) <= 4 *)
let rec solve nums v : string list =
  (* Recursive call cannot return a list because they cannot be on nums of length 4 *)
  let solve nums v = List.nth_opt (solve nums v) 0 in
  let check op =
    let other_input = apply (inverse op) v in
    let test l r = Option.is_some @@ solve l @@ other_input r in
    let concat_known_other l r =
      op_to_string (solve l @@ other_input r) (solve [ r ] r) op
      |> Option.to_list
    in
    let test_unknown_other l r =
      List.exists
        (fun i -> Option.is_some @@ solve l @@ other_input i)
        (possible_outputs r)
    in
    let concat_unknown_other l i =
      op_to_string (solve l @@ other_input i) (Some (Float.to_string i)) op
      |> Option.to_list
    in
    match nums with
    | [] -> []
    | [ x ] when x = v -> [ Float.to_string x ]
    (*Split off first of any size array*)
    | hd :: tl when test tl hd -> concat_known_other tl hd
    (*Split off second of any size array*)
    | a :: b :: tl when test (a :: tl) b -> concat_known_other (a :: tl) b
    (*Split off third of any size array*)
    | a :: b :: c :: tl when test (a :: b :: tl) c ->
        concat_known_other (a :: b :: tl) c
    (*Split off fourth of any size array*)
    | a :: b :: c :: d :: tl when test (a :: b :: c :: tl) d ->
        concat_known_other (a :: b :: c :: tl) d
    | [ a; b; c; d ] when test_unknown_other [ a; b ] [ c; d ] ->
        List.map (concat_unknown_other [ a; b ]) (possible_outputs [ c; d ])
        |> List.flatten
    | [ a; b; c; d ] when test_unknown_other [ a; d ] [ b; c ] ->
        List.map (concat_unknown_other [ a; d ]) (possible_outputs [ b; c ])
        |> List.flatten
    | [ a; b; c; d ] when test_unknown_other [ a; c ] [ b; d ] ->
        List.map (concat_unknown_other [ a; c ]) (possible_outputs [ b; d ])
        |> List.flatten
    | _ -> []
  in
  List.map check ops |> List.flatten

let rsolve nums v = List.nth_opt (solve nums v) 0
