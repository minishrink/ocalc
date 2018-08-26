
let _ =
  let run_program = ref true in
  let welcome_message =
"\tTo quit OCalculator, type \"exit\" (case insensitive) or hit CTRL+C"
  in
  print_endline welcome_message;

  while !run_program do
    Printf.printf " >> ";
    let input = read_line () in
    if String.lowercase_ascii input = "exit" then begin
      print_endline " > exiting OCalculator";
      run_program := false
    end else Calculator.Calc_parser.display input
  done

