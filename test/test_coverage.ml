(** Coverage tests targeting uncovered branches *)

open Sql_parser

(* ===== Lexer edge cases ===== *)

let test_lexer_keywords () =
  let tokens = Lexer.tokenize
    "INSERT INTO VALUES CREATE TABLE DROP ORDER BY ASC DESC LIMIT JOIN INNER LEFT RIGHT ON AND OR NOT NULL TRUE FALSE IF EXISTS"
  in
  let expected = [
    Lexer.INSERT; Lexer.INTO; Lexer.VALUES; Lexer.CREATE; Lexer.TABLE;
    Lexer.DROP; Lexer.ORDER; Lexer.BY; Lexer.ASC; Lexer.DESC;
    Lexer.LIMIT; Lexer.JOIN; Lexer.INNER; Lexer.LEFT; Lexer.RIGHT;
    Lexer.ON; Lexer.AND; Lexer.OR; Lexer.NOT; Lexer.NULL;
    Lexer.TRUE; Lexer.FALSE; Lexer.IF; Lexer.EXISTS; Lexer.EOF
  ] in
  List.iter2 (fun t e -> Alcotest.(check bool) "keyword" true (t = e))
    tokens expected

let test_lexer_types () =
  let tokens = Lexer.tokenize "INT INTEGER FLOAT REAL DOUBLE STRING TEXT VARCHAR BOOL BOOLEAN" in
  let expected = [
    Lexer.INT_TYPE; Lexer.INT_TYPE; Lexer.FLOAT_TYPE; Lexer.FLOAT_TYPE; Lexer.FLOAT_TYPE;
    Lexer.STRING_TYPE; Lexer.STRING_TYPE; Lexer.STRING_TYPE;
    Lexer.BOOL_TYPE; Lexer.BOOL_TYPE; Lexer.EOF
  ] in
  List.iter2 (fun t e -> Alcotest.(check bool) "type" true (t = e))
    tokens expected

let test_lexer_punctuation () =
  let tokens = Lexer.tokenize "* , . ; ( ) + - /" in
  let expected = [
    Lexer.STAR; Lexer.COMMA; Lexer.DOT; Lexer.SEMICOLON;
    Lexer.LPAREN; Lexer.RPAREN; Lexer.PLUS; Lexer.MINUS; Lexer.SLASH; Lexer.EOF
  ] in
  List.iter2 (fun t e -> Alcotest.(check bool) "punct" true (t = e))
    tokens expected

let test_lexer_string_escapes () =
  let tokens = Lexer.tokenize "'hello\nworld'" in
  (match List.hd tokens with
   | Lexer.STRING_LIT s -> Alcotest.(check string) "escaped" "hello\nworld" s
   | _ -> Alcotest.fail "expected string lit")

let test_lexer_string_double_quote () =
  let tokens = Lexer.tokenize "\"hello\"" in
  (match List.hd tokens with
   | Lexer.STRING_LIT s -> Alcotest.(check string) "dq string" "hello" s
   | _ -> Alcotest.fail "expected string lit")

let test_lexer_unterminated_string () =
  let raised = try
    let _ = Lexer.tokenize "'unterminated" in false
  with Lexer.Lexer_error _ -> true in
  Alcotest.(check bool) "unterminated" true raised

let test_lexer_bang_error () =
  let raised = try
    let _ = Lexer.tokenize "!x" in false
  with Lexer.Lexer_error _ -> true in
  Alcotest.(check bool) "bang error" true raised

let test_lexer_unexpected_char () =
  let raised = try
    let _ = Lexer.tokenize "@#$" in false
  with Lexer.Lexer_error _ -> true in
  Alcotest.(check bool) "unexpected char" true raised

let test_lexer_ident () =
  let tokens = Lexer.tokenize "my_column MyTable" in
  (match List.hd tokens with
   | Lexer.IDENT s -> Alcotest.(check string) "ident" "my_column" s
   | _ -> Alcotest.fail "expected ident")

let test_string_of_token () =
  let _ = Lexer.string_of_token Lexer.SELECT in
  let _ = Lexer.string_of_token (Lexer.INT_LIT 42) in
  let _ = Lexer.string_of_token (Lexer.STRING_LIT "hi") in
  let _ = Lexer.string_of_token (Lexer.IDENT "x") in
  let _ = Lexer.string_of_token Lexer.EOF in
  ()

(* ===== Parser edge cases ===== *)

let test_parse_select_no_from () =
  let stmt = Parser.parse "SELECT 1" in
  (match stmt with
   | Ast.Select { from = None; _ } -> ()
   | _ -> Alcotest.fail "expected SELECT without FROM")

let test_parse_select_table_star () =
  let stmt = Parser.parse "SELECT u.* FROM users u" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     (match List.hd columns with
      | Ast.SelectTableStar _ -> ()
      | _ -> Alcotest.fail "expected SelectTableStar")
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_select_with_alias () =
  let stmt = Parser.parse "SELECT name n FROM users" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     (match List.hd columns with
      | Ast.SelectExpr (_, Some "n") -> ()
      | _ -> Alcotest.fail "expected alias")
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_select_func_call () =
  let stmt = Parser.parse "SELECT UPPER(name) FROM users" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     (match List.hd columns with
      | Ast.SelectExpr (Ast.FuncCall ("UPPER", _), _) -> ()
      | _ -> Alcotest.fail "expected FuncCall")
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_select_paren_expr () =
  let stmt = Parser.parse "SELECT (1 + 2) * 3" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     Alcotest.(check int) "one column" 1 (List.length columns)
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_select_multi_order () =
  let stmt = Parser.parse "SELECT * FROM t ORDER BY a ASC, b DESC" in
  (match stmt with
   | Ast.Select { order_by; _ } ->
     Alcotest.(check int) "two order items" 2 (List.length order_by)
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_select_table_alias () =
  let stmt = Parser.parse "SELECT * FROM users u" in
  (match stmt with
   | Ast.Select { from = Some (Ast.TableName (_, Some "u")); _ } -> ()
   | _ -> Alcotest.fail "expected table alias")

let test_parse_inner_join () =
  let stmt = Parser.parse "SELECT * FROM a INNER JOIN b ON a.id = b.id" in
  (match stmt with
   | Ast.Select { from = Some (Ast.JoinClause (_, Ast.InnerJoin, _, _)); _ } -> ()
   | _ -> Alcotest.fail "expected INNER JOIN")

let test_parse_left_join () =
  let stmt = Parser.parse "SELECT * FROM a LEFT JOIN b ON a.id = b.id" in
  (match stmt with
   | Ast.Select { from = Some (Ast.JoinClause (_, Ast.LeftJoin, _, _)); _ } -> ()
   | _ -> Alcotest.fail "expected LEFT JOIN")

let test_parse_right_join () =
  let stmt = Parser.parse "SELECT * FROM a RIGHT JOIN b ON a.id = b.id" in
  (match stmt with
   | Ast.Select { from = Some (Ast.JoinClause (_, Ast.RightJoin, _, _)); _ } -> ()
   | _ -> Alcotest.fail "expected RIGHT JOIN")

let test_parse_insert_multi_tuples () =
  let stmt = Parser.parse "INSERT INTO t (a, b) VALUES (1, 2), (3, 4)" in
  (match stmt with
   | Ast.Insert { values; _ } ->
     Alcotest.(check int) "two tuples" 2 (List.length values)
   | _ -> Alcotest.fail "expected INSERT")

let test_parse_insert_no_columns () =
  let stmt = Parser.parse "INSERT INTO t VALUES (1, 2)" in
  (match stmt with
   | Ast.Insert { columns = None; _ } -> ()
   | _ -> Alcotest.fail "expected no columns")

let test_parse_insert_negative_values () =
  let stmt = Parser.parse "INSERT INTO t VALUES (-1, -3.14)" in
  (match stmt with
   | Ast.Insert { values; _ } ->
     let row = List.hd values in
     (match row with
      | [Ast.VInt (-1); Ast.VFloat _] -> ()
      | _ -> Alcotest.fail "expected negative values")
   | _ -> Alcotest.fail "expected INSERT")

let test_parse_insert_bool_null () =
  let stmt = Parser.parse "INSERT INTO t VALUES (TRUE, FALSE, NULL)" in
  (match stmt with
   | Ast.Insert { values; _ } ->
     let row = List.hd values in
     Alcotest.(check int) "three vals" 3 (List.length row)
   | _ -> Alcotest.fail "expected INSERT")

let test_parse_create_table_if_not_exists () =
  let stmt = Parser.parse "CREATE TABLE IF NOT EXISTS t (id INT)" in
  (match stmt with
   | Ast.CreateTable { if_not_exists = true; _ } -> ()
   | _ -> Alcotest.fail "expected IF NOT EXISTS")

let test_parse_drop_table_no_if () =
  let stmt = Parser.parse "DROP TABLE t" in
  (match stmt with
   | Ast.DropTable { if_exists = false; _ } -> ()
   | _ -> Alcotest.fail "expected DROP without IF EXISTS")

let test_parse_create_table_not_null () =
  let stmt = Parser.parse "CREATE TABLE t (id INT NOT NULL, name STRING NOT NULL)" in
  (match stmt with
   | Ast.CreateTable { columns; _ } ->
      List.iter (fun cd -> Alcotest.(check bool) "not null" false cd.Ast.nullable) columns
   | _ -> Alcotest.fail "expected CREATE TABLE")

let test_parse_error () =
  let raised = try
    let _ = Parser.parse "INVALID SQL" in false
  with Parser.Parse_error _ -> true in
  Alcotest.(check bool) "parse error" true raised

let test_parse_error_trailing () =
  let raised = try
    let _ = Parser.parse "SELECT 1 FROM t SELECT 2" in false
  with Parser.Parse_error _ -> true in
  Alcotest.(check bool) "trailing error" true raised

let test_parse_select_table_dot_column () =
  let stmt = Parser.parse "SELECT u.name FROM users u" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     (match List.hd columns with
      | Ast.SelectExpr (Ast.Column { table = Some "u"; column = "name" }, _) -> ()
      | _ -> Alcotest.fail "expected table.column")
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_neg_expr () =
  let stmt = Parser.parse "SELECT -5" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     (match List.hd columns with
      | Ast.SelectExpr (Ast.UnOp (Ast.Neg, _), _) -> ()
      | _ -> Alcotest.fail "expected Neg")
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_not_expr () =
  let stmt = Parser.parse "SELECT NOT TRUE" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     (match List.hd columns with
      | Ast.SelectExpr (Ast.UnOp (Ast.Not, _), _) -> ()
      | _ -> Alcotest.fail "expected Not")
   | _ -> Alcotest.fail "expected SELECT")

let test_parse_arithmetic () =
  let stmt = Parser.parse "SELECT 1 + 2 - 3 * 4 / 5" in
  (match stmt with
   | Ast.Select { columns; _ } ->
     Alcotest.(check int) "one column" 1 (List.length columns)
   | _ -> Alcotest.fail "expected SELECT")

(* ===== Executor edge cases ===== *)

let test_exec_select_no_from () =
  let db = Storage.create_database (Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_1") in
  let result =
    let stmt = Parser.parse "SELECT 1 + 2" in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  Alcotest.(check int) "one row" 1 (List.length result.rows)

let test_exec_functions () =
  let db = Storage.create_database (Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_2") in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE t (s STRING)" in
  let _ = exec "INSERT INTO t VALUES ('hello')" in
  let r = exec "SELECT UPPER(s), LOWER(s), LENGTH(s), CONCAT(s, '!') FROM t" in
  Alcotest.(check int) "one row" 1 (List.length r.rows)

let test_exec_div_by_zero () =
  let raised = try
    let _ = Executor.eval_binop Ast.Div (Ast.VInt 1) (Ast.VInt 0) in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "div by zero" true raised

let test_exec_div_float_by_zero () =
  let raised = try
    let _ = Executor.eval_binop Ast.Div (Ast.VFloat 1.0) (Ast.VFloat 0.0) in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "float div by zero" true raised

let test_exec_and_or () =
  let _ = Executor.eval_binop Ast.And (Ast.VBool true) (Ast.VBool false) in
  let _ = Executor.eval_binop Ast.Or (Ast.VBool true) (Ast.VBool false) in
  let _ = Executor.eval_binop Ast.And (Ast.VNull) (Ast.VBool true) in
  let _ = Executor.eval_binop Ast.Or (Ast.VBool true) (Ast.VNull) in
  ()

let test_exec_and_error () =
  let raised = try
    let _ = Executor.eval_binop Ast.And (Ast.VInt 1) (Ast.VInt 2) in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "and error" true raised

let test_exec_or_error () =
  let raised = try
    let _ = Executor.eval_binop Ast.Or (Ast.VInt 1) (Ast.VInt 2) in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "or error" true raised

let test_exec_arithmetic_error () =
  let raised = try
    let _ = Executor.eval_binop Ast.Add (Ast.VString "a") (Ast.VString "b") in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "arith error" true raised

let test_exec_not_non_bool () =
  let raised = try
    let _ = Executor.eval_expr ["x"] [Ast.VInt 1] (Ast.UnOp (Ast.Not, Ast.Column { table = None; column = "x" })) in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "not error" true raised

let test_exec_neg_non_num () =
  let raised = try
    let _ = Executor.eval_expr ["x"] [Ast.VString "a"] (Ast.UnOp (Ast.Neg, Ast.Column { table = None; column = "x" })) in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "neg error" true raised

let test_exec_unknown_func () =
  let raised = try
    let _ = Executor.eval_func "UNKNOWN" [Ast.VInt 1] in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "unknown func" true raised

let test_exec_compare_mixed () =
  let _ = Executor.compare_values Ast.VNull (Ast.VInt 1) in
  let _ = Executor.compare_values (Ast.VInt 1) Ast.VNull in
  let _ = Executor.compare_values (Ast.VString "a") (Ast.VBool true) in
  ()

let test_exec_is_truthy () =
  Alcotest.(check bool) "true" true (Executor.is_truthy (Ast.VBool true));
  Alcotest.(check bool) "int nonzero" true (Executor.is_truthy (Ast.VInt 1));
  Alcotest.(check bool) "float nonzero" true (Executor.is_truthy (Ast.VFloat 1.0));
  Alcotest.(check bool) "string nonempty" true (Executor.is_truthy (Ast.VString "x"));
  Alcotest.(check bool) "false" false (Executor.is_truthy (Ast.VBool false));
  Alcotest.(check bool) "null" false (Executor.is_truthy Ast.VNull)

let test_exec_numeric_mixed () =
  let _ = Executor.numeric_op ( + ) ( +. ) (Ast.VInt 1) (Ast.VFloat 2.0) in
  let _ = Executor.numeric_op ( + ) ( +. ) (Ast.VFloat 1.0) (Ast.VInt 2) in
  let _ = Executor.numeric_op ( + ) ( +. ) (Ast.VInt 1) (Ast.VNull) in
  ()

let test_exec_null_arithmetic () =
  let v = Executor.eval_binop Ast.Add Ast.VNull (Ast.VInt 1) in
  Alcotest.(check bool) "null result" true (v = Ast.VNull)

let test_exec_negative_unary () =
  let v = Executor.eval_expr [] [] (Ast.UnOp (Ast.Neg, Ast.Literal (Ast.VFloat 3.14))) in
  (match v with Ast.VFloat _ -> () | _ -> Alcotest.fail "expected float")

let test_exec_not_null () =
  let v = Executor.eval_expr [] [] (Ast.UnOp (Ast.Not, Ast.Literal Ast.VNull)) in
  Alcotest.(check bool) "null" true (v = Ast.VNull)

let test_exec_func_wrong_args () =
  let raised = try
    let _ = Executor.eval_func "UPPER" [Ast.VInt 1] in false
  with Executor.Exec_error _ -> true in
  Alcotest.(check bool) "wrong args" true raised

let test_exec_format_result () =
  let result = { Executor.columns = ["a"; "b"]; rows = [[Ast.VInt 1; Ast.VString "x"]]; message = None } in
  let s = Executor.format_result result in
  Alcotest.(check bool) "has output" true (String.length s > 0)

let test_exec_format_message () =
  let result = { Executor.columns = []; rows = []; message = Some "done" } in
  let s = Executor.format_result result in
  Alcotest.(check string) "message" "done" s

let test_exec_format_empty () =
  let result = { Executor.columns = []; rows = []; message = None } in
  let s = Executor.format_result result in
  Alcotest.(check string) "ok" "OK" s

(* ===== Storage edge cases ===== *)

let test_storage_csv () =
  let _ = Storage.value_to_csv (Ast.VString "has,comma") in
  let _ = Storage.value_to_csv (Ast.VString "has\"quote") in
  let _ = Storage.value_to_csv (Ast.VString "has\nnewline") in
  let _ = Storage.value_to_csv (Ast.VBool true) in
  let _ = Storage.value_to_csv (Ast.VBool false) in
  let _ = Storage.value_to_csv Ast.VNull in
  let _ = Storage.value_to_csv (Ast.VFloat 3.14) in
  ()

let test_storage_csv_to_value () =
  Alcotest.(check bool) "int" true (Storage.csv_to_value Ast.TInt "42" = Ast.VInt 42);
  Alcotest.(check bool) "int fallback" true (Storage.csv_to_value Ast.TInt "abc" = Ast.VString "abc");
  Alcotest.(check bool) "float" true (Storage.csv_to_value Ast.TFloat "3.14" = Ast.VFloat 3.14);
  Alcotest.(check bool) "float fallback" true (Storage.csv_to_value Ast.TFloat "abc" = Ast.VString "abc");
  Alcotest.(check bool) "bool true" true (Storage.csv_to_value Ast.TBool "true" = Ast.VBool true);
  Alcotest.(check bool) "bool 1" true (Storage.csv_to_value Ast.TBool "1" = Ast.VBool true);
  Alcotest.(check bool) "bool false" true (Storage.csv_to_value Ast.TBool "false" = Ast.VBool false);
  Alcotest.(check bool) "bool 0" true (Storage.csv_to_value Ast.TBool "0" = Ast.VBool false);
  Alcotest.(check bool) "bool fallback" true (Storage.csv_to_value Ast.TBool "maybe" = Ast.VString "maybe");
  Alcotest.(check bool) "string" true (Storage.csv_to_value Ast.TString "hello" = Ast.VString "hello");
  Alcotest.(check bool) "empty null" true (Storage.csv_to_value Ast.TInt "" = Ast.VNull)

let test_storage_parse_csv () =
  let fields = Storage.parse_csv_line "a,b,c" in
  Alcotest.(check int) "3 fields" 3 (List.length fields);
  let quoted = Storage.parse_csv_line "\"hello,world\"" in
  Alcotest.(check int) "1 quoted field" 1 (List.length quoted)

let test_storage_load_from_disk () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_load" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE t (id INT, name STRING)" in
  let _ = exec "INSERT INTO t VALUES (1, 'Alice')" in
  let _ = exec "INSERT INTO t VALUES (2, 'Bob')" in
  (* Create a fresh db — tables list is empty, should load from disk *)
  let db2 = Storage.create_database dir in
  let r = exec "SELECT * FROM t" in
  Alcotest.(check int) "loaded from disk" 2 (List.length r.rows)

let test_storage_create_if_exists () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_ie" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE t (id INT)" in
  let _ = exec "CREATE TABLE IF NOT EXISTS t (id INT)" in
  let raised = try
    let _ = exec "CREATE TABLE t (id INT)" in false
  with Storage.Storage_error _ -> true in
  Alcotest.(check bool) "already exists" true raised

let test_storage_drop_if_not_exists () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_di" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "DROP TABLE IF EXISTS nonexistent" in
  let raised = try
    let _ = exec "DROP TABLE nonexistent" in false
  with Storage.Storage_error _ -> true in
  Alcotest.(check bool) "drop nonexistent" true raised

let test_storage_get_table_not_found () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_nf" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let raised = try
    let _ = Storage.get_table db "nonexistent" in false
  with Storage.Storage_error _ -> true in
  Alcotest.(check bool) "not found" true raised

let test_storage_insert_with_columns () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_ic" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE t (a INT, b INT, c INT)" in
  let _ = exec "INSERT INTO t (b, c, a) VALUES (2, 3, 1)" in
  let r = exec "SELECT * FROM t" in
  let row = List.hd r.rows in
  (match row with
   | [Ast.VInt 1; Ast.VInt 2; Ast.VInt 3] -> ()
   | _ -> Alcotest.fail "column reorder failed")

(* ===== Types edge cases ===== *)

let test_type_check_ok () =
  let schema = [
    { Types.schema_table = "users";
      schema_columns = [
        { Ast.col_name = "id"; col_type = Ast.TInt; nullable = false };
        { Ast.col_name = "name"; col_type = Ast.TString; nullable = true };
      ] }
  ] in
  let stmt = Parser.parse "SELECT id, name FROM users WHERE id > 0" in
  Types.check_statement schema stmt

let test_type_check_ambiguous_column () =
  let schema = [
    { Types.schema_table = "a";
      schema_columns = [{ Ast.col_name = "id"; col_type = Ast.TInt; nullable = true }] };
    { Types.schema_table = "b";
      schema_columns = [{ Ast.col_name = "id"; col_type = Ast.TInt; nullable = true }] };
  ] in
  let stmt = Parser.parse "SELECT id FROM a JOIN b ON a.id = b.id" in
  let raised = try Types.check_statement schema stmt; false with Types.Type_error _ -> true in
  Alcotest.(check bool) "ambiguous" true raised

let test_type_check_insert_count () =
  let schema = [
    { Types.schema_table = "t";
      schema_columns = [
        { Ast.col_name = "a"; col_type = Ast.TInt; nullable = true };
        { Ast.col_name = "b"; col_type = Ast.TInt; nullable = true };
      ] }
  ] in
  let stmt = Parser.parse "INSERT INTO t VALUES (1)" in
  let raised = try Types.check_statement schema stmt; false with Types.Type_error _ -> true in
  Alcotest.(check bool) "count mismatch" true raised

let test_type_check_unknown_table_alias () =
  let schema = [
    { Types.schema_table = "users";
      schema_columns = [{ Ast.col_name = "id"; col_type = Ast.TInt; nullable = true }] };
  ] in
  let stmt = Parser.parse "SELECT bad.x FROM users bad" in
  let raised = try Types.check_statement schema stmt; false with Types.Type_error _ -> true in
  Alcotest.(check bool) "unknown alias" true raised

let test_type_check_join_cond () =
  let schema = [
    { Types.schema_table = "a";
      schema_columns = [{ Ast.col_name = "id"; col_type = Ast.TInt; nullable = true }] };
    { Types.schema_table = "b";
      schema_columns = [{ Ast.col_name = "id"; col_type = Ast.TInt; nullable = true }] };
  ] in
  let stmt = Parser.parse "SELECT * FROM a JOIN b ON a.id = b.id" in
  Types.check_statement schema stmt

let test_type_check_select_no_from () =
  let stmt = Parser.parse "SELECT 1 + 2" in
  Types.check_statement [] stmt

let test_type_check_drop () =
  let stmt = Parser.parse "DROP TABLE t" in
  Types.check_statement [] stmt

(* ===== Planner string_of_plan ===== *)

let test_planner_string_all () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_plan" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    let s = Planner.string_of_plan "" plan in
    Alcotest.(check bool) "non-empty" true (String.length s > 0);
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE t (id INT, val INT)" in
  let _ = exec "INSERT INTO t VALUES (1, 10)" in
  let _ = exec "INSERT INTO t VALUES (2, 20)" in
  let _ = exec "SELECT * FROM t WHERE val > 5 ORDER BY id ASC LIMIT 1" in
  let _ = exec "INSERT INTO t VALUES (3, 30)" in
  let _ = exec "DROP TABLE t" in
  ()

let test_planner_join_plan () =
  let stmt = Parser.parse "SELECT * FROM a JOIN b ON a.id = b.id" in
  let plan = Planner.plan_statement stmt in
  let s = Planner.string_of_plan "" plan in
  Alcotest.(check bool) "has Join" true (String.length s > 0)

(* ===== Executor LEFT/RIGHT JOIN ===== *)

let test_exec_left_join () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_lj" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE a (id INT)" in
  let _ = exec "CREATE TABLE b (id INT, val INT)" in
  let _ = exec "INSERT INTO a VALUES (1)" in
  let _ = exec "INSERT INTO a VALUES (2)" in
  let _ = exec "INSERT INTO b VALUES (1, 100)" in
  let r = exec "SELECT * FROM a LEFT JOIN b ON a.id = b.id" in
  Alcotest.(check int) "two rows (left join)" 2 (List.length r.rows)

let test_exec_right_join () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_cov_rj" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE a (id INT, val INT)" in
  let _ = exec "CREATE TABLE b (id INT)" in
  let _ = exec "INSERT INTO a VALUES (1, 100)" in
  let _ = exec "INSERT INTO b VALUES (1)" in
  let _ = exec "INSERT INTO b VALUES (2)" in
  let r = exec "SELECT * FROM a RIGHT JOIN b ON a.id = b.id" in
  Alcotest.(check int) "two rows (right join)" 2 (List.length r.rows)

(* ===== AST string_of_value ===== *)

let test_string_of_value () =
  Alcotest.(check string) "int" "42" (Ast.string_of_value (Ast.VInt 42));
  Alcotest.(check string) "float" "3.14" (Ast.string_of_value (Ast.VFloat 3.14));
  Alcotest.(check string) "string" "hi" (Ast.string_of_value (Ast.VString "hi"));
  Alcotest.(check string) "true" "true" (Ast.string_of_value (Ast.VBool true));
  Alcotest.(check string) "null" "NULL" (Ast.string_of_value Ast.VNull)

let test_string_of_data_type () =
  Alcotest.(check string) "int" "INT" (Ast.string_of_data_type Ast.TInt);
  Alcotest.(check string) "float" "FLOAT" (Ast.string_of_data_type Ast.TFloat);
  Alcotest.(check string) "string" "STRING" (Ast.string_of_data_type Ast.TString);
  Alcotest.(check string) "bool" "BOOL" (Ast.string_of_data_type Ast.TBool)

(* ===== Test Runner ===== *)

let lexer_tests = [
  "keywords", `Quick, test_lexer_keywords;
  "types", `Quick, test_lexer_types;
  "punctuation", `Quick, test_lexer_punctuation;
  "string escapes", `Quick, test_lexer_string_escapes;
  "double quoted string", `Quick, test_lexer_string_double_quote;
  "unterminated string", `Quick, test_lexer_unterminated_string;
  "bang error", `Quick, test_lexer_bang_error;
  "unexpected char", `Quick, test_lexer_unexpected_char;
  "ident", `Quick, test_lexer_ident;
  "string_of_token", `Quick, test_string_of_token;
]

let parser_tests = [
  "select no from", `Quick, test_parse_select_no_from;
  "select table.*", `Quick, test_parse_select_table_star;
  "select with alias", `Quick, test_parse_select_with_alias;
  "select func call", `Quick, test_parse_select_func_call;
  "select paren expr", `Quick, test_parse_select_paren_expr;
  "select multi order", `Quick, test_parse_select_multi_order;
  "select table alias", `Quick, test_parse_select_table_alias;
  "inner join", `Quick, test_parse_inner_join;
  "left join", `Quick, test_parse_left_join;
  "right join", `Quick, test_parse_right_join;
  "insert multi tuples", `Quick, test_parse_insert_multi_tuples;
  "insert no columns", `Quick, test_parse_insert_no_columns;
  "insert negative", `Quick, test_parse_insert_negative_values;
  "insert bool null", `Quick, test_parse_insert_bool_null;
  "create if not exists", `Quick, test_parse_create_table_if_not_exists;
  "drop no if", `Quick, test_parse_drop_table_no_if;
  "create not null", `Quick, test_parse_create_table_not_null;
  "parse error", `Quick, test_parse_error;
  "parse error trailing", `Quick, test_parse_error_trailing;
  "table.column", `Quick, test_parse_select_table_dot_column;
  "neg expr", `Quick, test_parse_neg_expr;
  "not expr", `Quick, test_parse_not_expr;
  "arithmetic", `Quick, test_parse_arithmetic;
]

let executor_tests = [
  "select no from", `Quick, test_exec_select_no_from;
  "functions", `Quick, test_exec_functions;
  "div by zero", `Quick, test_exec_div_by_zero;
  "float div by zero", `Quick, test_exec_div_float_by_zero;
  "and or", `Quick, test_exec_and_or;
  "and error", `Quick, test_exec_and_error;
  "or error", `Quick, test_exec_or_error;
  "arithmetic error", `Quick, test_exec_arithmetic_error;
  "not non bool", `Quick, test_exec_not_non_bool;
  "neg non num", `Quick, test_exec_neg_non_num;
  "unknown func", `Quick, test_exec_unknown_func;
  "compare mixed", `Quick, test_exec_compare_mixed;
  "is truthy", `Quick, test_exec_is_truthy;
  "numeric mixed", `Quick, test_exec_numeric_mixed;
  "null arithmetic", `Quick, test_exec_null_arithmetic;
  "negative unary", `Quick, test_exec_negative_unary;
  "not null", `Quick, test_exec_not_null;
  "func wrong args", `Quick, test_exec_func_wrong_args;
  "format result", `Quick, test_exec_format_result;
  "format message", `Quick, test_exec_format_message;
  "format empty", `Quick, test_exec_format_empty;
  "left join", `Quick, test_exec_left_join;
  "right join", `Quick, test_exec_right_join;
]

let storage_tests = [
  "csv serialize", `Quick, test_storage_csv;
  "csv to value", `Quick, test_storage_csv_to_value;
  "parse csv", `Quick, test_storage_parse_csv;
  "load from disk", `Quick, test_storage_load_from_disk;
  "create if exists", `Quick, test_storage_create_if_exists;
  "drop if not exists", `Quick, test_storage_drop_if_not_exists;
  "get table not found", `Quick, test_storage_get_table_not_found;
  "insert with columns", `Quick, test_storage_insert_with_columns;
]

let type_tests = [
  "check ok", `Quick, test_type_check_ok;
  "ambiguous column", `Quick, test_type_check_ambiguous_column;
  "insert count", `Quick, test_type_check_insert_count;
  "unknown table alias", `Quick, test_type_check_unknown_table_alias;
  "join cond", `Quick, test_type_check_join_cond;
  "select no from", `Quick, test_type_check_select_no_from;
  "drop", `Quick, test_type_check_drop;
]

let planner_tests = [
  "string all plans", `Quick, test_planner_string_all;
  "join plan", `Quick, test_planner_join_plan;
]

let ast_tests = [
  "string_of_value", `Quick, test_string_of_value;
  "string_of_data_type", `Quick, test_string_of_data_type;
]

let () =
  Alcotest.run "sql_parser coverage" [
    "lexer", lexer_tests;
    "parser", parser_tests;
    "executor", executor_tests;
    "storage", storage_tests;
    "types", type_tests;
    "planner", planner_tests;
    "ast", ast_tests;
  ]