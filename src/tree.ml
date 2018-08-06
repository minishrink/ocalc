
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

