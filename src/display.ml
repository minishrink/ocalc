
module P = Parser

(** Display helpers **)

let get_num = function
  | P.Num i -> i
  | x -> failwith (Printf.sprintf "get_num (%s)" P.(string_exp x))


let maybe_remove_dot s =
  let last = String.length s - 1 in
  if String.contains_from s last '.'
  then String.sub s 0 last
  else s

let display string =
  string
  |> P.interpret
  |> get_num |> string_of_float |> maybe_remove_dot
  |> Printf.sprintf " >> %s"
  |> print_endline

