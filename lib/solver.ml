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

let list_format l1 l2 op =
  let format a b =
    match op with
    | Add -> Printf.sprintf "(%s %s %s)" a "+" b
    | Sub -> Printf.sprintf "(%s %s %s)" a "-" b
    | Mul -> Printf.sprintf "(%s %s %s)" a "*" b
    | Div -> Printf.sprintf "(%s %s %s)" a "/" b
    | RSub -> Printf.sprintf "(%s %s %s)" b "-" a
    | RDiv -> Printf.sprintf "(%s %s %s)" b "/" a
  in
  let cartesian l1 l2 =
    List.map (fun o -> List.map (fun i -> (o, i)) l2) l1 |> List.flatten
  in
  let uncurry f (x, y) = f x y in
  List.map (fun x -> uncurry format x) @@ cartesian l1 l2

(* RDiv and RSub are Div and Sub with swapped inputs *)
let ops = [ Add; Sub; Mul; Div ]

(* Returns all possible combinations of operators and a list of two numbers *)
let possible_outputs =
  let module FloatSet = Set.Make (Float) in
  function
  | [ a; b ] ->
      List.map (fun op -> apply op a b) (RSub :: RDiv :: ops)
      |> FloatSet.of_list |> FloatSet.to_list
  | [ a ] -> [ a ]
  | _ -> failwith "List not of two elements"

(* Requires len(l) <= 4 *)
let split k u l =
  if List.length l > 4 then failwith "List length greater than 4";
  let four_way = function
    | [ a; b; c; d ] ->
        List.flatten
          [ u [ a; b ] [ c; d ]; u [ a; c ] [ b; d ]; u [ a; d ] [ b; c ] ]
    | _ -> []
  in
  let fourth = function
    | a :: b :: c :: d :: tl -> k (a :: b :: c :: tl) d
    | _ -> []
  in
  let third = function a :: b :: c :: tl -> k (a :: b :: tl) c | _ -> [] in
  let second = function a :: b :: tl -> k (a :: tl) b | _ -> [] in
  let first = function a :: tl -> k tl a | _ -> [] in

  List.fold_left
    (fun acc x -> List.append acc (x l))
    []
    [ four_way; fourth; third; second; first ]

(* Requires len(nums) <= 4 *)
let rec solve nums v : string list =
  if List.length nums > 4 then failwith "List length greater than 4";
  let inner op =
    let ( = ) a b = Float.abs (a -. b) <= 0.0001 in
    let round n = Float.round (n *. 1000.) /. 1000. in
    let other_input x = apply (inverse op) v x |> round in

    let combine_known l1 r =
      list_format (solve l1 @@ other_input r) (solve [ r ] r) op
    in
    let combine_unknown l1 l2 =
      List.fold_left
        (fun acc r ->
          List.append acc
          @@ list_format (solve l1 @@ other_input r) (solve l2 r) op)
        [] (possible_outputs l2)
    in

    match nums with
    | [] -> []
    | [ x ] when x = v -> [ Float.to_string x ]
    | l -> split combine_known combine_unknown l
  in
  let module StringSet = Set.Make (String) in
  List.map inner ops |> List.flatten |> StringSet.of_list |> StringSet.to_list
