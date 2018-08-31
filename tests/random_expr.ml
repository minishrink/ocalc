module L = Calculator.Lexer
module P = Calculator.Parser

let random_num () =
  Random.self_init ();
  let sign = if Random.bool () then (-1.) else (1.) in
  sign *. Random.(float 34093.) |> string_of_float

let ops = [| "+" ; "-" ; "*" ; "/" |]

let op_of_string = function
  | "+" -> L.Add
  | "-" -> L.Sub
  | "*" -> L.Mul
  | "/" -> L.Div
  | str -> failwith (Printf.sprintf "op_of_string (%s)" str)

let random_op () =
  Random.self_init ();
  let random_num = (Random.int 375021593) mod 4 in
  (ops.(random_num))

let make_expr () =
  Random.self_init ();
  let len = Random.int 5 in
  let rec exp i =
    if i > 0
    then (random_num ())::(random_op ())::(exp (i-1))
    else [(random_num ())]
  in exp len

let string_expr () = make_expr () |> String.concat " "

let string_to_token s =
  try
    s |> float_of_string |> P.to_n
  with _ ->
  try s |> op_of_string |> (fun o -> L.O o) with
    e -> raise e

let lex_string s = List.map string_to_token s

