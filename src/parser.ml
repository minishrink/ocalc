module L = Lexer

(** Data structures **)
type exp =
  | Num  : float     -> exp
  | EMul : exp * exp -> exp
  | EDiv : exp * exp -> exp
  | ESub : exp * exp -> exp
  | EAdd : exp * exp -> exp

(** Debugging helpers **)
let rec string_exp = function
  | Num i ->
    Printf.sprintf "Num(%s)" (string_of_float i)
  | EMul (exp, f) ->
    Printf.sprintf "EMul(%s, %s)" (string_exp exp) (string_exp f)
  | EAdd (lexp, rexp) ->
    Printf.sprintf "EAdd(%s, %s)" (string_exp lexp) (string_exp rexp)
  | ESub (lexp, rexp) ->
    Printf.sprintf "ESub(%s, %s)" (string_exp lexp) (string_exp rexp)
  | EDiv (lexp, rexp) ->
    Printf.sprintf "EDiv(%s, %s)" (string_exp lexp) (string_exp rexp)

let tknlst_to_str token_lst =
  List.map L.Print.string_of_token token_lst
  |> String.concat " "

exception Parsing_error of string
let parse_exn str = raise (Parsing_error str)
let parse_fail fn_name token_list =
  let tokens = tknlst_to_str token_list in
  let err = Printf.sprintf "%s [%s]" fn_name tokens in
  parse_exn err

(** Conversion helpers **)

let to_n n = L.N n
let to_num n = Num n
let num_to_n = L.(function
    | Num n -> N n
    | e -> parse_exn (Printf.sprintf "num_to_n (%s)" (string_exp e)))
let n_to_num = L.(function
    | N n -> Num n
    | other -> parse_fail "n_to_num" [other])

(** Precedence helpers **)

let precedence = L.(function
  | Add | Sub -> 0
  | Mul -> 1
  | Div -> 2)

let equal_prec current comp =
  (precedence comp) = current

(** Evaluation **)

let binary_expr lexp rexp = function
  | L.Add -> EAdd (lexp, rexp)
  | L.Mul -> EMul (lexp, rexp)
  | L.Div -> EDiv (lexp, rexp)
  | L.Sub -> ESub (lexp, rexp)

let eval e =
  let rec eval_exp = function
    | Num f -> f
    | EMul (x,y) -> ( *. ) (eval_exp x) (eval_exp y)
    | EAdd (x,y) -> ( +. ) (eval_exp x) (eval_exp y)
    | ESub (x,y) -> ( -. )  (eval_exp x) (eval_exp y)
    | EDiv (x,y) -> ( /. ) (eval_exp x) (eval_exp y)
  in e |> eval_exp |> to_num

(** Parsing logic **)

(* deduce precedence of and then reduce first expression,
 * return evaluated expression * remaining token list *)
let reduce token_list =
  let open L in
  let rec red prec exp = function
    | (O o) :: (N n) :: lst when equal_prec prec o ->
      red prec (binary_expr exp (to_num n) o) lst
    | ((O _) :: _) | [] as lst ->
      exp |> eval, lst
    | other -> parse_fail "reduce" other
  in match token_list with
  | (N n)::(O o)::tkns -> red (precedence o) (to_num n) ((O o)::tkns)
  | [N n] -> to_num n, []
  | other -> parse_fail "reduce" other

(* pass through entire token list
 * extract sublist containing expressions of given precedence
 * replace with reduced value and return list of lower precedence *)
let rec parse_by_prec for_prec = L.(function
  | (N _) :: (O o) :: _ as lst when equal_prec for_prec o ->
    let exp, remaining_tokens = reduce lst in
    parse_by_prec for_prec
      ((num_to_n exp)::remaining_tokens)
  | (N _ as n) :: (O _ as op) :: tokens ->
    n::op::(parse_by_prec for_prec tokens)
  | [ N _ ] as n -> n
  | other -> parse_fail "parse_by_prec" other)

(* successively reduce expressions in token list by descending order of precedence *)
let parse token_list =
  token_list
  |> parse_by_prec 2 (* reduce expressions of precedence 2 *)
  |> parse_by_prec 1 (* reduce expressions of precedence 1 *)
  |> reduce |> fst   (* returned expr of precedence 0, now reduce *)

(** CLI helpers **)

let interpret string =
  string
  |> L.lex
  |> parse
  |> eval


