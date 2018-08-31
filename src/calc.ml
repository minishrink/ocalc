module C = Calculator

let run_program = ref true

let setup () =
  let welcome_message =
    " || Type \"exit\" (case insensitive) or hit CTRL+C to quit"
  in
  print_endline welcome_message

let run () =
  Printf.printf " << ";
  let input = read_line () in
  if String.lowercase_ascii input = "exit" then begin
    print_endline " >> exiting OCalc";
    run_program := false
  end else C.Calc_parser.display input

let string_exn = C.(function
    | Calc_lexer.Lexing_error e ->
      Printf.sprintf "Lexing error: %s" e
    | Calc_parser.Parsing_error e ->
      Printf.sprintf "Parsing error: %s" e
    | e -> raise e)

let safely_run () =
  setup ();
  while !run_program do
    try
      run ()
    with
    | C.Calc_lexer.Lexing_error _
    | C.Calc_parser.Parsing_error _ as e ->
      e |> string_exn |> print_endline
    | e -> raise e
  done

let _ = safely_run ()
