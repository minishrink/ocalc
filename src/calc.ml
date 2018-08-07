
let _ =
  let run_program = ref true in
  let welcome_message =
    "\n\tOCalc, MyCalc! Enter a binary arithmetic expression to calculate.
\tTo quit this program, type only the word \"EXIT\" or hit ctrl+c"
  in
  print_endline welcome_message;

  while !run_program do
    Printf.printf "\t\tINPUT: ";
    let input = read_line () in
    if input = "EXIT" then begin
      print_endline "\n\tOCalc OTerminated";
      run_program := false
    end else Calculator.Calc_parser.print_test input
  done

