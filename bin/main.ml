module Writer : sig
  type 'a t = 'a * string

  val return : 'a -> 'a t
  val join : 'a t -> 'b t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val deref : 'a t -> 'a
  val log : 'a t -> string
end = struct
  type 'a t = 'a * string

  let return x = (x, "")
  let join (_, s1) (y, s2) = (y, s1 ^ s2)
  let bind ((x, _) as m) f = join m @@ f x
  let deref (a, _) = a
  let log (_, b) = b
end

type op = Add | Sub | Mul | Div

let apply = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div ->
      fun a b -> Int.to_float a /. Int.to_float b |> Float.round |> Float.to_int

let inverse = function Add -> Sub | Sub -> Add | Mul -> Div | Div -> Mul
let to_string = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
let nums = [ 1; 3; 4; 7 ]
let ops = [ Add; Sub; Mul; Div ]
let ( >>= ) = Writer.bind
let ( >+ ) = Writer.join
let ( ! ) = Writer.deref

let rec solve (nums : int list) v : bool Writer.t =
  let other_input op = apply (inverse op) v in
  let check op : bool Writer.t =
    match nums with
    | [] -> failwith "Empty arr"
    | [ x ] -> (x = v, Int.to_string x)
    | x :: tl when !(solve tl @@ other_input op x) ->
        solve [ x ] x
        >+ (true, Printf.sprintf ")%s(" @@ to_string op)
        >+ solve tl @@ other_input op x
    | [ x; y; z ] when !(solve [ x; y ] @@ other_input op z) ->
        solve [ z ] z
        >+ (true, Printf.sprintf ")%s(" @@ to_string op)
        >+ solve [ x; y ] @@ other_input op z
    | [ x; y; z ] when !(solve [ x; z ] @@ other_input op y) ->
        solve [ y ] y
        >+ (true, Printf.sprintf ")%s(" @@ to_string op)
        >+ solve [ x; z ] @@ other_input op y
    | _ -> Writer.return false
  in
  List.map check ops |> List.hd
