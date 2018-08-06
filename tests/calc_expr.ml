
let add_exprs =
  [ "1 + 3",           4.
  ; "2.0 + 9.0",        11.
  ; "-8 + 7",        -1.
  ; "6 + -98",       -92.
  ; "-285 + -19",   -304.
  ; "0.1 + 5.53",      5.54
  ; "6 + 1.45",        7.4
  ; "63.2 + 0.14",   63.34
  ; "-1.4 + 6",       4.6
  ; "12 + -9.2",      2.8
  ; "-19.1 + -2.5", -21.6
  ]

let test_float = Alcotest.float 5.

let test_add_exprs () =
  List.iter
    (fun (string, result) ->
       Alcotest.(check test_float)
         "Addition"
         Calculator.Calc_parser.(string |> parse_test |> function | Value f -> f)
         result
    )
    add_exprs

let tests =
  [ "Test addition", `Quick, test_add_exprs
  ]
