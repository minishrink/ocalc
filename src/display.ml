
module P = Parser

let get_num = function
  | P.Num i -> i
  | x       -> failwith (Printf.sprintf "get_num (%s)" P.(string_exp x))

let maybe_remove_dot s =
  let last = String.length s - 1 in
  if   String.contains_from s last '.'
  then String.sub s 0 last
  else s

let handle_float f =
  let undefined_value () = raise P.(Arithmetic_error "Undefined value") in
  let infinity_check f   = if f=infinity then undefined_value () else f in
  let check_nan f = match Pervasives.classify_float f with
    | Pervasives.FP_nan -> undefined_value ()
    | _ -> f
  in
  f |> infinity_check |> check_nan

(** Variables **)

let prefix_input  = "  | "
let prefix_output = "  ="
let prefix_msg    = " ||"

let display_string prefix s =
  Printf.sprintf "%s %s" prefix s

let display_input  = display_string prefix_input
let display_output = display_string prefix_output

let calculate expr =
  expr
  |> P.interpret
  |> get_num
  |> handle_float
  |> string_of_float |> maybe_remove_dot
  |> display_output
  |> Printf.printf "%s\n"

