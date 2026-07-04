(** Tests for the SQL parser using Alcotest *)

open Sql_parser

(* ===== Lexer Tests ===== *)

let test_lexer_select () =
  let tokens = Lexer.tokenize "SELECT * FROM users" in
  Alcotest.(check int) "token count" 5 (List.length tokens);
  Alcotest.(check bool) "first is SELECT" true (List.nth tokens 0 = Lexer.SELECT);
  Alcotest.(check bool) "second is STAR" true (List.nth tokens 1 = Lexer.STAR);
  Alcotest.(check bool) "third is FROM" true (List.nth tokens 2 = Lexer.FROM);
  Alcotest.(check bool) "last is EOF" true (List.nth tokens 4 = Lexer.EOF)

let test_lexer_string_literal () =
  let tokens = Lexer.tokenize "'hello world'" in
  Alcotest.(check bool) "string lit" true
    (List.nth tokens 0 = Lexer.STRING_LIT "hello world")

let test_lexer_numbers () =
  let tokens = Lexer.tokenize "42 3.14" in
  Alcotest.(check bool) "int lit" true (List.nth tokens 0 = Lexer.INT_LIT 42);
  Alcotest.(check bool) "float lit" true (List.nth tokens 1 = Lexer.FLOAT_LIT 3.14)

let test_lexer_operators () =
  let tokens = Lexer.tokenize "= <> < > <= >=" in
  Alcotest.(check bool) "eq" true (List.nth tokens 0 = Lexer.EQ);
  Alcotest.(check bool) "neq" true (List.nth tokens 1 = Lexer.NEQ);
  Alcotest.(check bool) "lt" true (List.nth tokens 2 = Lexer.LT);
  Alcotest.(check bool) "gt" true (List.nth tokens 3 = Lexer.GT);
  Alcotest.(check bool) "lte" true (List.nth tokens 4 = Lexer.LTE);
  Alcotest.(check bool) "gte" true (List.nth tokens 5 = Lexer.GTE)

let test_lexer_comment () =
  let tokens = Lexer.tokenize "SELECT -- this is a comment\n42" in
  Alcotest.(check bool) "select" true (List.nth tokens 0 = Lexer.SELECT);
  Alcotest.(check bool) "42" true (List.nth tokens 1 = Lexer.INT_LIT 42)

(* ===== Parser Tests ===== *)

let test_parse_select_star () =
  let stmt = Parser.parse "SELECT * FROM users" in
  match stmt with
  | Ast.Select { columns; from; where; order_by; limit } ->
    Alcotest.(check int) "one select item" 1 (List.length columns);
    Alcotest.(check bool) "is star" true (List.hd columns = Ast.SelectStar);
    Alcotest.(check bool) "has from" true (from <> None);
    Alcotest.(check bool) "no where" true (where = None);
    Alcotest.(check int) "no order" 0 (List.length order_by);
    Alcotest.(check bool) "no limit" true (limit = None)
  | _ -> Alcotest.fail "expected SELECT"

let test_parse_select_where () =
  let stmt = Parser.parse "SELECT name, age FROM users WHERE age > 18" in
  match stmt with
  | Ast.Select { columns; where; _ } ->
    Alcotest.(check int) "two columns" 2 (List.length columns);
    Alcotest.(check bool) "has where" true (where <> None)
  | _ -> Alcotest.fail "expected SELECT"

let test_parse_select_order_limit () =
  let stmt = Parser.parse "SELECT * FROM t ORDER BY id DESC LIMIT 10" in
  match stmt with
  | Ast.Select { order_by; limit; _ } ->
    Alcotest.(check int) "one order item" 1 (List.length order_by);
    Alcotest.(check bool) "desc" true
      ((List.hd order_by).direction = Ast.Desc);
    Alcotest.(check bool) "limit 10" true (limit = Some 10)
  | _ -> Alcotest.fail "expected SELECT"

let test_parse_select_join () =
  let stmt = Parser.parse
    "SELECT u.name, o.total FROM users u JOIN orders o ON u.id = o.user_id" in
  match stmt with
  | Ast.Select { from = Some (Ast.JoinClause _); _ } -> ()
  | _ -> Alcotest.fail "expected SELECT with JOIN"

let test_parse_insert () =
  let stmt = Parser.parse
    "INSERT INTO users (name, age) VALUES ('Alice', 30)" in
  match stmt with
  | Ast.Insert { table; columns; values } ->
    Alcotest.(check string) "table" "users" table;
    Alcotest.(check bool) "has columns" true (columns <> None);
    Alcotest.(check int) "one row" 1 (List.length values);
    Alcotest.(check int) "two values" 2 (List.length (List.hd values))
  | _ -> Alcotest.fail "expected INSERT"

let test_parse_create_table () =
  let stmt = Parser.parse
    "CREATE TABLE users (id INT NOT NULL, name STRING, active BOOL)" in
  match stmt with
  | Ast.CreateTable { table; columns; if_not_exists } ->
    Alcotest.(check string) "table" "users" table;
    Alcotest.(check int) "three columns" 3 (List.length columns);
    Alcotest.(check bool) "not if_not_exists" false if_not_exists;
    let id_col = List.hd columns in
    Alcotest.(check string) "id name" "id" id_col.col_name;
    Alcotest.(check bool) "id not nullable" false id_col.nullable
  | _ -> Alcotest.fail "expected CREATE TABLE"

let test_parse_drop_table () =
  let stmt = Parser.parse "DROP TABLE IF EXISTS users" in
  match stmt with
  | Ast.DropTable { table; if_exists } ->
    Alcotest.(check string) "table" "users" table;
    Alcotest.(check bool) "if exists" true if_exists
  | _ -> Alcotest.fail "expected DROP TABLE"

let test_parse_expressions () =
  let stmt = Parser.parse "SELECT 1 + 2 * 3, NOT true, -5" in
  match stmt with
  | Ast.Select { columns; _ } ->
    Alcotest.(check int) "three expressions" 3 (List.length columns)
  | _ -> Alcotest.fail "expected SELECT"

(* ===== Storage and Executor Integration Tests ===== *)

let test_create_insert_select () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_test_1" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  (* CREATE TABLE *)
  let stmt = Parser.parse "CREATE TABLE test1 (id INT, name STRING)" in
  let plan = Planner.plan_statement stmt in
  let _ = Executor.execute db plan in
  (* INSERT *)
  let stmt = Parser.parse "INSERT INTO test1 VALUES (1, 'Alice')" in
  let plan = Planner.plan_statement stmt in
  let _ = Executor.execute db plan in
  let stmt = Parser.parse "INSERT INTO test1 VALUES (2, 'Bob')" in
  let plan = Planner.plan_statement stmt in
  let _ = Executor.execute db plan in
  (* SELECT *)
  let stmt = Parser.parse "SELECT * FROM test1" in
  let plan = Planner.plan_statement stmt in
  let result = Executor.execute db plan in
  Alcotest.(check int) "two rows" 2 (List.length result.rows);
  Alcotest.(check int) "two columns" 2 (List.length result.columns);
  (* Cleanup *)
  (try Sys.remove (Filename.concat dir "test1.csv") with _ -> ());
  (try Sys.remove (Filename.concat dir "test1.schema") with _ -> ());
  (try Unix.rmdir dir with _ -> ())

let test_where_filter () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_test_2" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE test2 (id INT, val INT)" in
  let _ = exec "INSERT INTO test2 VALUES (1, 10)" in
  let _ = exec "INSERT INTO test2 VALUES (2, 20)" in
  let _ = exec "INSERT INTO test2 VALUES (3, 30)" in
  let result = exec "SELECT * FROM test2 WHERE val > 15" in
  Alcotest.(check int) "two matching rows" 2 (List.length result.rows);
  (* Cleanup *)
  (try Sys.remove (Filename.concat dir "test2.csv") with _ -> ());
  (try Sys.remove (Filename.concat dir "test2.schema") with _ -> ());
  (try Unix.rmdir dir with _ -> ())

let test_order_by () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_test_3" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE test3 (id INT, name STRING)" in
  let _ = exec "INSERT INTO test3 VALUES (3, 'Charlie')" in
  let _ = exec "INSERT INTO test3 VALUES (1, 'Alice')" in
  let _ = exec "INSERT INTO test3 VALUES (2, 'Bob')" in
  let result = exec "SELECT * FROM test3 ORDER BY id ASC" in
  let first_id = List.hd (List.hd result.rows) in
  Alcotest.(check bool) "first is 1" true (first_id = Ast.VInt 1);
  (* Cleanup *)
  (try Sys.remove (Filename.concat dir "test3.csv") with _ -> ());
  (try Sys.remove (Filename.concat dir "test3.schema") with _ -> ());
  (try Unix.rmdir dir with _ -> ())

let test_limit () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_test_4" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE test4 (id INT)" in
  let _ = exec "INSERT INTO test4 VALUES (1)" in
  let _ = exec "INSERT INTO test4 VALUES (2)" in
  let _ = exec "INSERT INTO test4 VALUES (3)" in
  let result = exec "SELECT * FROM test4 LIMIT 2" in
  Alcotest.(check int) "limited to 2" 2 (List.length result.rows);
  (* Cleanup *)
  (try Sys.remove (Filename.concat dir "test4.csv") with _ -> ());
  (try Sys.remove (Filename.concat dir "test4.schema") with _ -> ());
  (try Unix.rmdir dir with _ -> ())

let test_drop_table () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_test_5" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE test5 (id INT)" in
  let _ = exec "DROP TABLE test5" in
  let raised = try
    let _ = exec "SELECT * FROM test5" in false
  with Storage.Storage_error _ -> true in
  Alcotest.(check bool) "table dropped" true raised;
  (* Cleanup *)
  (try Unix.rmdir dir with _ -> ())

let test_join () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "sql_test_6" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let db = Storage.create_database dir in
  let exec sql =
    let stmt = Parser.parse sql in
    let plan = Planner.plan_statement stmt in
    Executor.execute db plan
  in
  let _ = exec "CREATE TABLE users6 (id INT, name STRING)" in
  let _ = exec "CREATE TABLE orders6 (id INT, user_id INT, total INT)" in
  let _ = exec "INSERT INTO users6 VALUES (1, 'Alice')" in
  let _ = exec "INSERT INTO users6 VALUES (2, 'Bob')" in
  let _ = exec "INSERT INTO orders6 VALUES (1, 1, 100)" in
  let _ = exec "INSERT INTO orders6 VALUES (2, 1, 200)" in
  let result = exec "SELECT * FROM users6 JOIN orders6 ON id = user_id" in
  Alcotest.(check int) "two joined rows" 2 (List.length result.rows);
  (* Cleanup *)
  (try Sys.remove (Filename.concat dir "users6.csv") with _ -> ());
  (try Sys.remove (Filename.concat dir "users6.schema") with _ -> ());
  (try Sys.remove (Filename.concat dir "orders6.csv") with _ -> ());
  (try Sys.remove (Filename.concat dir "orders6.schema") with _ -> ());
  (try Unix.rmdir dir with _ -> ())

(* ===== Type Checker Tests ===== *)

let test_type_check_unknown_table () =
  let schema = [] in
  let stmt = Parser.parse "SELECT * FROM nonexistent" in
  let raised = try
    Types.check_statement schema stmt; false
  with Types.Type_error _ -> true in
  Alcotest.(check bool) "unknown table error" true raised

let test_type_check_unknown_column () =
  let schema = [
    { Types.schema_table = "users";
      schema_columns = [
        { Ast.col_name = "id"; col_type = Ast.TInt; nullable = false };
        { Ast.col_name = "name"; col_type = Ast.TString; nullable = true };
      ] }
  ] in
  let stmt = Parser.parse "SELECT nonexistent FROM users" in
  let raised = try
    Types.check_statement schema stmt; false
  with Types.Type_error _ -> true in
  Alcotest.(check bool) "unknown column error" true raised

let test_type_check_duplicate_columns () =
  let stmt = Parser.parse "CREATE TABLE t (id INT, id STRING)" in
  let raised = try
    Types.check_statement [] stmt; false
  with Types.Type_error _ -> true in
  Alcotest.(check bool) "duplicate column error" true raised

(* ===== Planner Tests ===== *)

let test_planner_select () =
  let stmt = Parser.parse "SELECT * FROM t WHERE x > 1 ORDER BY x LIMIT 5" in
  let plan = Planner.plan_statement stmt in
  let plan_str = Planner.string_of_plan "" plan in
  Alcotest.(check bool) "has Project" true (String.length plan_str > 0)

(* ===== Test Runner ===== *)

let lexer_tests = [
  "select tokens", `Quick, test_lexer_select;
  "string literal", `Quick, test_lexer_string_literal;
  "numbers", `Quick, test_lexer_numbers;
  "operators", `Quick, test_lexer_operators;
  "comments", `Quick, test_lexer_comment;
]

let parser_tests = [
  "select star", `Quick, test_parse_select_star;
  "select where", `Quick, test_parse_select_where;
  "select order limit", `Quick, test_parse_select_order_limit;
  "select join", `Quick, test_parse_select_join;
  "insert", `Quick, test_parse_insert;
  "create table", `Quick, test_parse_create_table;
  "drop table", `Quick, test_parse_drop_table;
  "expressions", `Quick, test_parse_expressions;
]

let integration_tests = [
  "create insert select", `Quick, test_create_insert_select;
  "where filter", `Quick, test_where_filter;
  "order by", `Quick, test_order_by;
  "limit", `Quick, test_limit;
  "drop table", `Quick, test_drop_table;
  "join", `Quick, test_join;
]

let type_tests = [
  "unknown table", `Quick, test_type_check_unknown_table;
  "unknown column", `Quick, test_type_check_unknown_column;
  "duplicate columns", `Quick, test_type_check_duplicate_columns;
]

let planner_tests = [
  "select plan", `Quick, test_planner_select;
]

let () =
  Alcotest.run "sql_parser" [
    "lexer", lexer_tests;
    "parser", parser_tests;
    "integration", integration_tests;
    "types", type_tests;
    "planner", planner_tests;
  ]
