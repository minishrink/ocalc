module L = Calc_lexer
module T = Tree

module Calc = struct
  open L
  type t = token
  let string_of_operand = function
    | Add  -> "+"
    | Sub -> "-"
    | Div   -> "/"
    | Mult  -> "*"

  let string_of = function
    | Num(Int i)   -> Printf.sprintf "Int(%d)" i
    | Num(Float f) -> Printf.sprintf "Float(%f)" f
    | Op o -> Printf.sprintf "Op(%s)" (string_of_operand o)


  let get_num = function
    | L.Int i -> float_of_int i
    | L.Float f -> f

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

(* FIXME : this does not actually work but er, it does at least build a tree?
 * basically it has no precedence levels *)
open L
open CP
(* FIXME here's where we need precedence levels *)
let rec eval = function
  | [] -> CP.Empty
  | [ Num _ as x ] -> build x ()
  | [ Num _ as x ; Op o ; Num _ as y ] -> tree_exp o x y
  | (Num _ as x)::(Op o)::rest -> begin
      match o with
      | (Div | Mult | Sub) -> begin match rest with
          | (Num _ as a)::(Op m)::rst ->
            build
              ~left:(tree_exp o x a)
              (Op m)
              ~right:(eval rst)
              ()
          | _ -> failwith "should not reach this!" end
      (* FIXME : wtf doesn't work for negative numbers??? *)
      | Add ->
        build
          ~left:(build x ())
          (Op o)
          ~right:(eval rest)
          ()
    end
  | _ -> failwith "expecting either [Number], [Number ; Op ; Number] or [Number ; Op ; ( etc )]"



let parse tree =
  let open CP in
  let open L in
  let rec eval = function
    | Empty, Num x, Empty -> Calc.get_num x
    | Node x, Op o, Node y -> Calc.(get_op o) (eval x) (eval y)
    | _, _, _ -> failwith ("EVAL: " ^ (string_of tree))
  in Value (tree |> split |> eval)

let parse_and_print tree =
  tree
  |> parse
  |> string_of_val
  |> fun s -> print_endline (Printf.sprintf "\t\t\t%s" s)

let build_tree lst =
  if (List.length lst) mod 2 = 0 then
    failwith "FAULTY EXPR: only expecting odd number of tokens";
  eval lst

let test string =
  string
  |> L.lex
  |> List.map (function | L.Monad.Success x -> x | _ -> failwith "FUCK")
  |> build_tree

let parse_test s = test s |> parse
let print_test s = test s |> parse_and_print

