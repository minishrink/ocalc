module C = Calculator
module P = C.Parser
module D = C.Display

let run_program = ref true

let exit_program () = run_program := false

let setup () =
  let cutoff_bar = " --------------------------------------------------------" in
  let title_text = " ||      OCALC" in
  Printf.printf
    "%s\n%s\n%s\n%s Type \"exit\" (case insensitive) or hit CTRL+C to quit\n%s\n"
    cutoff_bar title_text cutoff_bar D.prefix_msg cutoff_bar

let run () =
  Printf.printf "%s" D.prefix_input;
  let input = read_line () in
  if String.lowercase_ascii input = "exit"
  then begin
    print_endline D.(display_string prefix_msg "exiting OCalc");
    exit_program ()
  end else
    D.calculate input

let string_exn = C.(function
    | Lexer.Lexing_error e ->
      Printf.sprintf "Lexing error: %s" e
    | Parser.Parsing_error e ->
      Printf.sprintf "Parsing error: %s" e
    | Parser.Arithmetic_error e ->
      Printf.sprintf "Arithmetic error: %s" e
    | e -> raise e)

let safely_run () =
  setup ();
  while !run_program do
    try
      run ()
    with
    | C.Lexer.Lexing_error _
    | C.Parser.Parsing_error _
    | C.Parser.Arithmetic_error _ as e ->
      e |> string_exn |> print_endline
    | e -> raise e
  done

let _ = safely_run ()
