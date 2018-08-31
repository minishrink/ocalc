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
  end else C.Parser.display input

let string_exn = C.(function
    | Lexer.Lexing_error e ->
      Printf.sprintf "Lexing error: %s" e
    | Parser.Parsing_error e ->
      Printf.sprintf "Parsing error: %s" e
    | e -> raise e)

let safely_run () =
  setup ();
  while !run_program do
    try
      run ()
    with
    | C.Lexer.Lexing_error _
    | C.Parser.Parsing_error _ as e ->
      e |> string_exn |> print_endline
    | e -> raise e
  done

let _ = safely_run ()
