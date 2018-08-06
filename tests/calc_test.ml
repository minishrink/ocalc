
let () =
  Alcotest.run "Full expression test"
    [ "Test expressions", Calc_expr.tests
    ]
