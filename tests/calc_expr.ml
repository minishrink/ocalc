
module C = Calculator
module L = C.Lexer
module P = C.Parser
module D = C.Display
module R = Random_expr

let testable pp =
  let pp = Fmt.of_to_string pp in
  let equal = (=) in
  Alcotest.testable pp equal
let token = testable L.Print.string_of_token

let add_exprs =
  [ "1 + 3",           4.
  ; "2.0 + 9.0",        11.
  ; "-8 + 7",        -1.
  ; "6 + -98",       -92.
  ; "-285 + -19",   -304.
  ; "0.1 + 5.53",      5.63
  ; "6 + 1.45",        7.45
  ; "63.2 + 0.14",   63.34
  ; "-1.4 + 6",       4.6
  ; "12 + -9.2",      2.8
  ; "-19.1 + -2.5", -21.6
  ]

let mixed_exprs =
  [ "5 + 4 - 6 * 17"       ,   (-93.)
  (* ; "35 / 2 - 2985 - -0.83", (-2966.67) *) (* FIXME *)
  ]

let test_float msg expected actual =
  Alcotest.(check string) msg (string_of_float expected) (string_of_float actual)

let test_add_exprs () =
  List.iter
    (fun (string, result) ->
       test_float
         "Addition"
         result
         (string |> P.interpret |> D.get_num)
    )
    add_exprs

let test_against_result (query,expected) =
  let actual = query
             |> P.interpret
             |> D.get_num
  in
  test_float
    "Calculation expression"
    expected actual

let test_mixed_exprs () =
  List.iter test_against_result mixed_exprs

let check_lex () =
  let expr = R.make_expr () in
  let expr_str = expr |> String.concat " " in
  let tkns = R.lex_string expr in
  let lexed = L.lex expr_str in
  print_endline expr_str;
  let print tokens = tokens |> P.tknlst_to_str |> print_endline in
  print tkns;
  print lexed;

  Alcotest.(check (list token))
    "Check expressions are correctly lexed"
    tkns lexed

let test_lexing () =
  for i = 0 to 20 do
    ignore i;
    check_lex ()
  done

let tests =
  [ "Test addition", `Quick, test_add_exprs
  ; "Mixed calculation expressions", `Quick, test_mixed_exprs
  ; "Lexing expressions", `Quick, test_lexing
  ]
