module L = Calc_lexer
module T = Tree

module Calc = struct
  type t = L.token
  let string_of_operand = L.Print.string_of_operand

  let string_of = L.Print.string_of_token

  let get_op = L.(function
      | Add  -> ( +. )
      | Sub  -> ( -. )
      | Div  -> ( /. )
      | Mult -> ( *. ))
end
module CP = T.Make(Calc)

type value = Value of float
let string_of_val = function
  | Value f -> (string_of_float f)

let tree_exp op x y =
  CP.(build
        ~left:(build x ())
        (Op op)
        ~right:(build y ())
        ())

open L
open CP

let simple_tree x o y = build ~left:(build x ()) o ~right:(build y ()) ()
let singleroot x = build x ()

(* FIXME still shit *)
let rec eval = function
  | [] -> Empty
  | [ Num _ as j ] -> singleroot j
  | [ Num _ as n ; Op _ as o ; Num _ as p ] -> simple_tree n o p
  | (Num _ as x) :: (Op _ as op) :: (Num _ as y) :: rst ->
    begin match rst with
      | (Op _ as nxt_op) :: rest ->
        build
          ~left:(simple_tree x op y)
          nxt_op
          ~right:(eval rest)
          ()
      | rest -> eval rest
    end
  | lst -> failwith
             (Printf.sprintf
                "Could not parse tokens: %s"
                (List.map Calc.string_of lst |> String.concat ","))

let parse ast =
  let open CP in
  let open L in
  let rec eval = function
    | Empty, Num x, Empty -> x
    | Node x, Op o, Node y -> Calc.(get_op o) (eval x) (eval y)
    | _, _, _ -> failwith ("could not parse: " ^ (string_of ast))
  in Value (ast |> split |> eval)

let parse_and_print tree =
  tree
  |> parse
  |> string_of_val
  |> fun s -> print_endline (Printf.sprintf "\t\t\t%s" s)

let build_tree lst =
  if (List.length lst) mod 2 = 0 then
    failwith "FAULTY EXPR: only expecting odd number of tokens";
  lst (* |> List.rev *) |> eval

let test string =
  string
  |> L.lex
  |> List.map (function | L.Monad.Success x -> x | _ -> failwith "FUCK")
  |> build_tree

let parse_test s = test s |> parse
let print_test s = test s |> parse_and_print

