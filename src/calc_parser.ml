module L = Calc_lexer

type exp =
  | Num   : float       -> exp
  | EMul : exp * exp   -> exp
  | EDiv : exp * exp   -> exp
  | ESub : exp * exp   -> exp
  | EAdd : exp * exp   -> exp

let rec string_exp = function
  | Num f ->
    Printf.sprintf "Num(%s)" (string_of_float f)
  | EMul (exp, f) ->
    Printf.sprintf "EMul(%s, %s)" (string_exp exp) (string_exp f)
  | EAdd (lexp, rexp) ->
    Printf.sprintf "EAdd(%s, %s)" (string_exp lexp) (string_exp rexp)
  | ESub (lexp, rexp) ->
    Printf.sprintf "ESub(%s, %s)" (string_exp lexp) (string_exp rexp)
  | EDiv (lexp, rexp) ->
    Printf.sprintf "EDiv(%s, %s)" (string_exp lexp) (string_exp rexp)


let to_n x = Num x
let eval e =
  let rec eval_exp = function
    | Num f -> f
    | EMul (x,y) -> ( *. ) (eval_exp x) (eval_exp y)
    | EAdd (x,y) -> ( +. ) (eval_exp x) (eval_exp y)
    | ESub (x,y) -> ( -. ) (eval_exp x) (eval_exp y)
    | EDiv (x,y) -> ( /. ) (eval_exp x) (eval_exp y)
  in e |> eval_exp |> to_n

let parse token_list =
  let open L in
  let open Print in
  let precedence = function
    | Add | Sub -> 0
    | Mul -> 1
    | Div -> 2
  in
  let make_exp x y = function
    | Add -> EAdd (x,y)
    | Sub -> ESub (x,y)
    | Div -> EDiv (x,y)
    | Mul -> EMul (x,y)
  in
  let to_num = function
    | Num f -> N f
    | x -> failwith (Printf.sprintf "%s::[%s]" (string_exp x) (tknlst_to_str token_list))
  in

  let rec reduce lvl = function
    | (N n)::(Op o)::lst when (precedence o) = lvl ->
      let lexp, rexp = reduce lvl lst in
      make_exp (to_n n) lexp o, rexp

    | (N n)::(([] | (Op _)::_) as tokens) ->
      to_n n, tokens
    | x ->
      failwith (Printf.sprintf "reduce (%d) [%s]" lvl (tknlst_to_str x))
  in

  let rec exp lvl = function
    | [ N x ] -> to_n x

    | (N x)::(Op o)::tokens when (precedence o) = lvl ->
      make_exp (to_n x) (exp lvl tokens) o

    | (N _)::(Op o)::_ as lst when (precedence o) > lvl ->
      let lexp, token_list = reduce (precedence o) lst in
      let num = lexp |> eval |> to_num in
      let lvl = begin match token_list with
        | Op o::_ -> precedence o
        | _ -> lvl
      end in
      exp lvl (num::token_list)

    | (N x)::((Op o)::_) as lst -> (* precedence o < lvl, clearly *)
      make_exp (to_n x) (exp (precedence o) lst) o

    | other -> failwith (Printf.sprintf "exp (precedence %d) [%s]" lvl (tknlst_to_str other))

  in match token_list with
  | [ N x ] -> to_n x
  (* find precedence level *)
  | (N _)::(Op o)::_ as list ->
    exp (precedence o) list
  | other -> failwith (Printf.sprintf "build_expr [%s]" (tknlst_to_str other))

let get_float = function
  | Num f -> f
  | x -> failwith (Printf.sprintf "get_float (%s)" (string_exp x))

let interpret string =
  string
  |> L.lex
  |> List.map L.Monad.(function | Success x -> x | Failure _ -> failwith "fuck")
  |> parse
  |> eval

let display string =
  string
  |> interpret
  |> get_float |> string_of_float
  |> Printf.sprintf " > %s"
  |> print_endline

