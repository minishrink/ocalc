module Helpers = struct
  (* inclusive *)
  let in_between a x y = a >= x && a <= y
  let in_range c = in_between (Char.code c)

  (* string -> char list *)
  let string_to_char_lst input =
    let len = String.length input in
    let rec char i =
      if i < len then input.[i]::(char (i+1)) else []
    in char 0

  (* char tests *)
  let is_alpha c = in_range c 65 90 || in_range c 97 122
  let is_num c = in_range c 48 57
  let is_alphanum c = is_alpha c || is_num c
end
module H = Helpers

exception Lexing_error of string * string
let fail_lex fn_name str =
  raise (Lexing_error (fn_name, str))

type operand = Add | Mul | Sub | Div
type token = N of float | O of operand

let get_operand str =
  let wrap_op x = (O x) in
  let to_op : string -> operand = function
    | "+" -> Add
    | "*" -> Mul
    | "-" -> Sub
    | "/" -> Div
    | _ -> fail_lex "to_op" str
  in
  try to_op str |> wrap_op with _ ->
    fail_lex "get_operand" str

let get_number str =
  let is_int str =
    let contains_only_numbers input =
      List.fold_left (fun bool chr -> bool && H.is_num chr) true input
    in match H.string_to_char_lst str with
    | hd :: (_::_ as num) when hd='-' -> contains_only_numbers num
    | number -> contains_only_numbers number
  in
  try
    if is_int str
    then N (float_of_string str)
    else match String.split_on_char '.' str with
      | [num ; dem] when is_int num && is_int dem ->
        N (float_of_string str)
      | _ -> fail_lex "get_number" str
  with
  | e -> raise e

let tokenise str =
  try get_number str with
  | Lexing_error _ -> get_operand str
  | e -> raise e


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

(* Handle strings with no spaces *)
let ops = ['+';'-';'*';'/']
let nums = (List.init 10 (fun i -> i + 48 |> Char.chr))
let contains_operands_and_numbers string =
  let contains_elems_of =
    List.fold_left (fun x y -> x || (String.contains string y)) false
  in
  not (String.contains string ' ') && contains_elems_of ops && contains_elems_of nums

(* here we handle strings without spaces *)
let maybe_add_spaces string =
  let space = ' ' in
  let rec space_out = function
    | num :: op :: rest when (List.mem num nums && List.mem op ops) ->
      num::space::op::space::(space_out rest)
    | [ n ] as lst when List.mem n nums -> lst
    | [] -> []
    | hd :: tl -> hd::(space_out tl)
  in
  if contains_operands_and_numbers string
  then
    string |> H.string_to_char_lst
    |> space_out |> List.map Char.escaped
    |> String.concat ""
  else string

let lex str =
  str
  |> maybe_add_spaces
  |> String.split_on_char ' '
  |> List.filter (fun s -> not (s = "" || s = " "))
  |> List.map tokenise

