(** SQL Parser REPL *)

let db = Sql_parser.Storage.create_database "data"

let execute_sql input =
  try
    let stmt = Sql_parser.Parser.parse input in
    (* Type check if we have schema info *)
    (try
       let schema = Sql_parser.Storage.get_schema db in
       Sql_parser.Types.check_statement schema stmt
     with
     | Sql_parser.Types.Type_error msg ->
       Printf.printf "Type error: %s\n%!" msg;
       raise Exit
     | Sql_parser.Storage.Storage_error _ ->
       (* Table might not exist yet for CREATE TABLE *)
       ());
    let plan = Sql_parser.Planner.plan_statement stmt in
    let result = Sql_parser.Executor.execute db plan in
    print_string (Sql_parser.Executor.format_result result);
    print_newline ()
  with
  | Exit -> ()
  | Sql_parser.Lexer.Lexer_error msg ->
    Printf.printf "Lexer error: %s\n%!" msg
  | Sql_parser.Parser.Parse_error msg ->
    Printf.printf "Parse error: %s\n%!" msg
  | Sql_parser.Executor.Exec_error msg ->
    Printf.printf "Execution error: %s\n%!" msg
  | Sql_parser.Storage.Storage_error msg ->
    Printf.printf "Storage error: %s\n%!" msg

let () =
  Printf.printf "SQL Parser REPL\n";
  Printf.printf "Type SQL statements followed by Enter. Type 'quit' or 'exit' to leave.\n\n%!";
  let buf = Buffer.create 256 in
  try while true do
    if Buffer.length buf = 0 then
      Printf.printf "sql> %!"
    else
      Printf.printf "  -> %!";
    let line = input_line stdin in
    let trimmed = String.trim line in
    if Buffer.length buf = 0 &&
       (String.lowercase_ascii trimmed = "quit" ||
        String.lowercase_ascii trimmed = "exit") then
      raise Exit;
    Buffer.add_string buf line;
    Buffer.add_char buf ' ';
    (* Execute when we see a semicolon at end or a complete statement *)
    let content = String.trim (Buffer.contents buf) in
    if String.length content > 0 &&
       (content.[String.length content - 1] = ';' ||
        (* Single-line statements without semicolons *)
        (not (String.contains content ';') &&
         let upper = String.uppercase_ascii content in
         (String.length upper >= 4 &&
          (String.sub upper 0 4 = "QUIT" ||
           String.sub upper 0 4 = "EXIT")))) then begin
      execute_sql content;
      Buffer.clear buf
    end
  done with
  | Exit | End_of_file ->
    Printf.printf "\nBye!\n"
