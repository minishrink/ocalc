module C = Calculator
module P = C.Parser
module D = C.Display

let run_program = ref true
let debug = ref false

let setup () =
  let welcome_message =
    " || Type \"exit\" (case insensitive) or hit CTRL+C to quit"
  in
  print_endline welcome_message

let run () =
  Printf.printf " << ";
  let input = read_line () in
  match String.lowercase_ascii input with
  | "exit" -> begin
      print_endline " >> exiting OCalc";
      run_program := false
    end
  | "debug-on"  -> debug := true
  | "debug-off" -> debug := false
  | input -> D.display input

let maybe_debug fn_name =
  if !debug then ", see function " ^ fn_name else ""

let string_exn = C.(function
    | Lexer.Lexing_error (fn, s) ->
      Printf.sprintf "Could not lex \"%s\"%s" s (maybe_debug fn)
    | Parser.Parsing_error (fn, s) ->
      Printf.sprintf "Could not parse \"%s\"%s" s (maybe_debug fn)
    | Parser.Arithmetic_error e
      -> "Cannot divide by zero: " ^ e
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
