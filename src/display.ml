module L = Lexer
module P = Parser

let get_num = function
  | P.Num i -> i
  | x -> failwith (Printf.sprintf "get_num (%s)" P.(string_exp x))

let handle_float f =
  let undefined_value () = raise P.(Arithmetic_error "Undefined value") in
  let infinity_check f =
    if f=infinity then undefined_value () else f
  in
  let check_nan f = match Pervasives.classify_float f with
    | Pervasives.FP_nan -> undefined_value ()
    | _ -> f
  in
  f |> infinity_check |> check_nan

let display string =
  string
  |> P.interpret
  |> get_num
  |> handle_float
  |> string_of_float |> L.Print.maybe_remove_dot
  |> Printf.sprintf " >> %s"
  |> print_endline

