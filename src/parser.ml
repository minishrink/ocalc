module L = Lexer

type exp =
  | Num  : float     -> exp
  | EMul : exp * exp -> exp
  | EDiv : exp * exp -> exp
  | ESub : exp * exp -> exp
  | EAdd : exp * exp -> exp

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

let to_num n = Num n
let num_to_n = L.(function | Num n -> N n | e -> parse_exn (Printf.sprintf "num_to_n (%s)" (string_exp e)))
let eval e =
  let rec eval_exp = function
    | Num f -> f
    | EMul (x,y) -> ( *. ) (eval_exp x) (eval_exp y)
    | EAdd (x,y) -> ( +. ) (eval_exp x) (eval_exp y)
    | ESub (x,y) -> ( -. )  (eval_exp x) (eval_exp y)
    | EDiv (x,y) -> ( /. ) (eval_exp x) (eval_exp y)
  in e |> eval_exp |> to_num


let precedence = L.(function
  | Add | Sub -> 0
  | Mul -> 1
  | Div -> 2)

let equal_prec current comp =
  (precedence comp) = current
let lowest_precedence = equal_prec 0

let binary_expr lexp rexp = function
  | L.Add -> EAdd (lexp, rexp)
  | L.Mul -> EMul (lexp, rexp)
  | L.Div -> EDiv (lexp, rexp)
  | L.Sub -> ESub (lexp, rexp)

let n_to_num = L.(function | N n -> Num n | other -> parse_fail "n_to_num" [other])
let hom_lst_to_exp op = L.(function
  | hd :: tl ->
    tl
    |> List.filter (function | N _ -> true | _ -> false)
    |> List.fold_left (fun x y -> binary_expr x (n_to_num y) op) (n_to_num hd)
  | [] -> parse_fail "hom_lst_to_exp" [])

(* take a homogeneous list and return an expression *)
let naive_expr of_lst =
  let open L in
  let rec expr so_far = function
    | (O o) :: (N n) :: rest -> expr (binary_expr so_far (to_num n) o) rest
    | [] -> so_far
    | other -> parse_fail "naive_expr" other
  in match of_lst with
  | (N n)::tkns -> expr (to_num n) tkns
  | other -> parse_fail "naive_expr" other

let rec expr_of prec = L.(function
  | (N _ as n) :: (O o as op) :: lst when equal_prec prec o ->
    let same_prec, remaining = expr_of prec lst in
    n::op::same_prec, remaining
  | (N n) :: rest -> [N n], rest
  | other -> parse_fail "expr_of" other
  )


(* take heterogeneous list and reduce exprs of specified precedence *)
let rec reduce_token_list for_prec = L.(function
  | (N _) :: (O o) :: _ as lst when equal_prec for_prec o ->
    let to_reduce, remaining_tokens = expr_of (precedence o) lst in
    reduce_token_list for_prec
      ((to_reduce |> hom_lst_to_exp o |> eval |> num_to_n)::remaining_tokens)

  | (N _ as n) :: (O _ as op) :: tokens ->
    n::op::(reduce_token_list for_prec tokens)
  | [ N _ ] as n -> n
  | other -> parse_fail "reduce_token_list" other)

(* heterogeneous token list -> expr *)
let parse token_list =
  token_list
  |> reduce_token_list 2
  |> reduce_token_list 1
  |> naive_expr

let get_num = function
  | Num i -> i
  | x -> failwith (Printf.sprintf "get_float (%s)" (string_exp x))

let interpret string =
  string
  |> L.lex
  |> List.map L.Monad.(function | Success x -> x | Failure _ -> failwith "fuck")
  |> parse
  |> eval

let maybe_remove_dot s =
  let last = String.length s - 1 in
  if String.contains_from s last '.'
  then String.sub s 0 last
  else s

let display string =
  string
  |> interpret
  |> get_num |> string_of_float |> maybe_remove_dot
  |> Printf.sprintf " >> %s"
  |> print_endline

