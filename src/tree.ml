
module type Branch = sig
  type t
  val string_of : t -> string
end

module Make(T : Branch) = struct
  type tree =
    | Empty
    | Node of (tree * T.t * tree)

  let rec string_of = function
    | Empty -> "_"
    | Node (Empty, mid, Empty) ->
      Printf.sprintf "Node(%s)"
        (T.string_of mid)
    | Node (Empty, mid, right) ->
      Printf.sprintf "Node((%s) --> %s)"
        (T.string_of mid) (string_of right)
    | Node (left, mid, Empty) ->
      Printf.sprintf "(%s <-- Node(%s))"
        (string_of left) (T.string_of mid)
    | Node (left, mid, right) ->
      Printf.sprintf "(%s <-- Node(%s) --> %s)"
        (string_of left) (T.string_of mid) (string_of right)

  let build ?(left=Empty) (middle : T.t) ?(right=Empty) () =
    Node (left, middle, right)

  let get_left = function
    | Node (left, _, _) -> left
    | Empty -> Empty

  let get_right = function
    | Node (_,_,right) -> right
    | Empty -> Empty

  let get_root = function
    | Node (_, root, _) -> Some root
    | _ -> None

  let unbox = function | Some x -> x | None -> Empty
  let unbox_root = function
    | Some x -> x
    | None -> failwith "unbox_root"

  let split tree =
    get_left tree, get_root tree |> unbox_root, get_right tree

  let rec get_lefts tree = match split tree with
    | Empty, r, _ -> [r]
    | left, r, _ -> r::(get_lefts left)

  let rec get_rights tree = match split tree with
    | Empty, r, _ -> [r]
    | right, r, _ -> r::(get_rights right)

  let get_depth tree =
    let rec depth i direction = function
      | Empty -> i
      | Node _ as x -> depth (i+1) direction x
    in
    let left_depth = depth 0 get_left tree in
    let right_depth = depth 0 get_right tree in
    max left_depth right_depth

end

module Int_Branch = struct
  type t = int
  let string_of = string_of_int
end

module IT = Make(Int_Branch)

let dummy = IT.build 0 ()

module BinExp = struct
  type op = Add | Mult | Sub | Div
  type number = Int of int | Float of float
  type t = Num of number | Op of op

  let string_op = function
    | Add -> "(+)"
    | Sub  -> "(-)"
    | Div  -> "(/)"
    | Mult -> "(x)"

  let string_of = function
    | Num (Int i) -> Printf.sprintf "Int(%s)" (string_of_int i)
    | Num (Float f) -> Printf.sprintf "Float(%s)" (string_of_float f)
    | Op o -> Printf.sprintf "Op%s" (string_op o)

  let get_num = function
    | Int i -> float_of_int i
    | Float f -> f

  let get_op = function
    | Add  -> ( +. )
    | Sub  -> ( -. )
    | Div  -> ( /. )
    | Mult -> ( *. )
end


module BT = Make(BinExp)

type value = Val of float

let parse tree =
  let open BT in
  let rec eval = function
    | Empty, BinExp.Num x, Empty -> BinExp.get_num x
    | Node x, BinExp.Op o, Node y -> BinExp.(get_op o) (eval x) (eval y)
    | _, _, _ -> failwith ("EVAL: " ^ (string_of tree))
  in Val (tree |> split |> eval)

let addexpr =
  let open BT in
  let open BinExp in
  let just_val x = Node(Empty, x, Empty) in
  build
    ~left:(build
             ~left:(just_val (Num (Int 6)))
             (Op Div)
             ~right:(just_val (Num (Float 3.)))
             () )
    (Op Add)
    ~right:(build
              ~left:(build
                       ~left:(just_val (Num (Int 2)))
                       (Op Mult)
                       ~right:(just_val (Num (Int 1)))
                       () )
              (Op Sub)
              ~right:(just_val (Num (Float 7.)))
              () )
    ()

open BT
open BinExp

let string_of_val = function
  | Val f -> Printf.sprintf "Val (%s)" (string_of_float f)

let parse_and_print tree =
  tree
  |> parse
  |> string_of_val
  |> print_endline

let tree_exp op x y =
  let tre =
    build
      ~left:(build x ())
      (Op op)
      ~right:(build y ())
      ()
  in parse_and_print tre; tre

(* FIXME : this does not actually work but er, it does at least build a tree?
 * basically it has no precedence levels *)
let rec eval = function
  | [] -> Empty
  | [ Num _ as x ] -> build x ()
  | [ Num _ as x ; Op o ; Num _ as y ] -> tree_exp o x y
  | (Num _ as x)::(Op o)::rest -> begin
      (* FIXME here's where we need precedence levels *)
      match o with
      | (Div | Mult) -> begin match rest with
          | (Num _ as a)::(Op m)::rst ->
            build ~left:(tree_exp o x a) (Op m) ~right:(eval rst) ()
          | _ -> failwith "should not reach this!"
        end
      | (Add | Sub) ->
        (* FIXME : wtf doesn't work for negative numbers??? *)
        build ~left:(build x ()) (Op o) ~right:(eval rest) ()
    end
  | _ -> failwith "expecting either [Number], [Number ; Op ; Number] or [Number ; Op ; ( etc )]"


let build_tree lst =
  if (List.length lst) mod 2 = 0 then failwith "FAULTY EXPR: only expecting odd number of tokens";
  eval lst

