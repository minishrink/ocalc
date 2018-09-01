module Helpers = struct
  (* inclusive *)
  let in_between a x y = a >= x && a <= y
  let in_range c = in_between (Char.code c)

  (* string -> char list *)
  let string_to_char_lst input =
    let rec char i =
      if i < String.length input then input.[i]::(char (i+1)) else []
    in char 0

  (* char tests *)
  let is_alpha c = in_range c 65 90 || in_range c 97 122
  let is_num c = in_range c 48 57
  let is_alphanum c = is_alpha c || is_num c
end
module H = Helpers

module Monad = struct
  type ('a, 'b) result =
    | Success of 'a
    | Failure of 'b

  let succeed x = Success x
  let fail y    = Failure y

  (* (('a, 'b) result -> ('a, 'b) result) -> ('a, 'b) result -> ('a, 'b) result *)
  let bind fn = function
    | Success a -> begin
        match fn a with
        | exception e -> Failure(e)
        | result -> succeed result
      end
    | Failure _ as fail -> fail

  let if_fail fn = function
    | Success _ as succ -> succ
    | Failure b -> fn b

  let (>>=) a fn = bind fn a
  let (<-<) a fn = if_fail fn a
end
open Monad


exception Lexing_error of string
let fail_lex _loc_ str = raise (Lexing_error (Printf.sprintf "%s\n%s" str _loc_))

type operand = Add | Mul | Sub | Div
type token = N of float | O of operand

let get_operand str =
  let wrap_op x = succeed (O x) in
  let to_op : string -> operand = function
    | "+" -> Add
    | "*" -> Mul
    | "-" -> Sub
    | "/" -> Div
    | _ -> fail_lex __LOC__ str
  in
  try to_op str |> wrap_op with _ ->
    fail_lex __LOC__ (Printf.sprintf "Failed to lex \"%s\"" str)

let get_number str =
  let is_int str =
    let contains_only_numbers input =
      List.fold_left (fun bool chr -> bool && H.is_num chr) true input
    in
    match H.string_to_char_lst str with
    | hd :: (_::_ as num) when hd='-' -> contains_only_numbers num
    | number -> contains_only_numbers number
  in
  try
    if is_int str
    then succeed (N (float_of_string str))
    else match String.split_on_char '.' str with
      | [num ; dem] when is_int num && is_int dem ->
        succeed (N (float_of_string str))
      | _ -> fail (Lexing_error str)
  with
  | e -> fail e

let tokenise str =
  get_number str
  <-< (function
      | Lexing_error str -> get_operand str
      | _ -> fail_lex __LOC__ str)


let return = function
  | Success a -> a
  | Failure f -> fail_lex __LOC__ f

module Print = struct
  let string_of_operand = function
    | Add -> "+"
    | Mul -> "*"
    | Sub -> "-"
    | Div -> "/"

  let string_of_token = function
    | (O x) -> string_of_operand x
    | (N x) -> string_of_float x

  let tknlst_to_str of_list =
    List.map string_of_token of_list
    |> String.concat " "
end

let lex str =
  str
  |> String.split_on_char ' '
  |> List.filter (fun s -> not (s = "" || s = " "))
  |> List.map tokenise

let string_of_lexed = function
  | Success y -> Print.string_of_token y
  | Failure _ -> fail_lex __LOC__ "cannot reach"

