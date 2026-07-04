(** Recursive descent SQL parser *)

open Ast

type parser_state = {
  tokens : Lexer.token array;
  mutable pos : int;
}

exception Parse_error of string

let create tokens =
  { tokens = Array.of_list tokens; pos = 0 }

let peek ps =
  if ps.pos < Array.length ps.tokens then ps.tokens.(ps.pos)
  else Lexer.EOF

let advance ps =
  if ps.pos < Array.length ps.tokens then
    ps.pos <- ps.pos + 1

let expect ps tok =
  let current = peek ps in
  if current = tok then advance ps
  else raise (Parse_error (Printf.sprintf "expected %s but got %s"
    (Lexer.string_of_token tok) (Lexer.string_of_token current)))

let expect_ident ps =
  match peek ps with
  | Lexer.IDENT s -> advance ps; s
  | tok -> raise (Parse_error (Printf.sprintf "expected identifier but got %s"
    (Lexer.string_of_token tok)))

(* Parse a primary expression *)
let rec parse_primary ps =
  match peek ps with
  | Lexer.INT_LIT i -> advance ps; Literal (VInt i)
  | Lexer.FLOAT_LIT f -> advance ps; Literal (VFloat f)
  | Lexer.STRING_LIT s -> advance ps; Literal (VString s)
  | Lexer.TRUE -> advance ps; Literal (VBool true)
  | Lexer.FALSE -> advance ps; Literal (VBool false)
  | Lexer.NULL -> advance ps; Literal VNull
  | Lexer.NOT -> advance ps; let e = parse_primary ps in UnOp (Not, e)
  | Lexer.MINUS -> advance ps; let e = parse_primary ps in UnOp (Neg, e)
  | Lexer.LPAREN ->
    advance ps;
    let e = parse_expr ps in
    expect ps Lexer.RPAREN;
    e
  | Lexer.IDENT name ->
    advance ps;
    (match peek ps with
     | Lexer.LPAREN ->
       (* function call *)
       advance ps;
       let args = parse_expr_list ps in
       expect ps Lexer.RPAREN;
       FuncCall (name, args)
     | Lexer.DOT ->
       advance ps;
       let col = expect_ident ps in
       Column { table = Some name; column = col }
     | _ ->
       Column { table = None; column = name })
  | tok ->
    raise (Parse_error (Printf.sprintf "unexpected token in expression: %s"
      (Lexer.string_of_token tok)))

and parse_expr_list ps =
  if peek ps = Lexer.RPAREN then []
  else begin
    let first = parse_expr ps in
    let rest = ref [] in
    while peek ps = Lexer.COMMA do
      advance ps;
      rest := parse_expr ps :: !rest
    done;
    first :: List.rev !rest
  end

(* Parse multiplication/division *)
and parse_mul ps =
  let left = parse_primary ps in
  let rec loop left =
    match peek ps with
    | Lexer.STAR -> advance ps; let right = parse_primary ps in loop (BinOp (Mul, left, right))
    | Lexer.SLASH -> advance ps; let right = parse_primary ps in loop (BinOp (Div, left, right))
    | _ -> left
  in
  loop left

(* Parse addition/subtraction *)
and parse_add ps =
  let left = parse_mul ps in
  let rec loop left =
    match peek ps with
    | Lexer.PLUS -> advance ps; let right = parse_mul ps in loop (BinOp (Add, left, right))
    | Lexer.MINUS -> advance ps; let right = parse_mul ps in loop (BinOp (Sub, left, right))
    | _ -> left
  in
  loop left

(* Parse comparison *)
and parse_comparison ps =
  let left = parse_add ps in
  match peek ps with
  | Lexer.EQ -> advance ps; let right = parse_add ps in BinOp (Eq, left, right)
  | Lexer.NEQ -> advance ps; let right = parse_add ps in BinOp (Neq, left, right)
  | Lexer.LT -> advance ps; let right = parse_add ps in BinOp (Lt, left, right)
  | Lexer.GT -> advance ps; let right = parse_add ps in BinOp (Gt, left, right)
  | Lexer.LTE -> advance ps; let right = parse_add ps in BinOp (Lte, left, right)
  | Lexer.GTE -> advance ps; let right = parse_add ps in BinOp (Gte, left, right)
  | _ -> left

(* Parse AND *)
and parse_and ps =
  let left = parse_comparison ps in
  let rec loop left =
    match peek ps with
    | Lexer.AND -> advance ps; let right = parse_comparison ps in loop (BinOp (And, left, right))
    | _ -> left
  in
  loop left

(* Parse OR (lowest precedence) *)
and parse_expr ps =
  let left = parse_and ps in
  let rec loop left =
    match peek ps with
    | Lexer.OR -> advance ps; let right = parse_and ps in loop (BinOp (Or, left, right))
    | _ -> left
  in
  loop left

(* Parse a select item *)
let parse_select_item ps =
  match peek ps with
  | Lexer.STAR -> advance ps; SelectStar
  | Lexer.IDENT name ->
    (let saved = ps.pos in
     advance ps;
     match peek ps with
     | Lexer.DOT ->
       advance ps;
       (match peek ps with
        | Lexer.STAR -> advance ps; SelectTableStar name
        | _ ->
          let col = expect_ident ps in
          let alias =
            match peek ps with
            | Lexer.IDENT a -> advance ps; Some a
            | _ -> None
          in
          SelectExpr (Column { table = Some name; column = col }, alias))
     | _ ->
       ps.pos <- saved;
       let e = parse_expr ps in
       let alias =
         match peek ps with
         | Lexer.IDENT a when String.uppercase_ascii a <> "FROM"
                           && String.uppercase_ascii a <> "WHERE"
                           && String.uppercase_ascii a <> "ORDER"
                           && String.uppercase_ascii a <> "LIMIT" ->
           advance ps; Some a
         | _ -> None
       in
       SelectExpr (e, alias))
  | _ ->
    let e = parse_expr ps in
    let alias =
      match peek ps with
      | Lexer.IDENT a when String.uppercase_ascii a <> "FROM" ->
        advance ps; Some a
      | _ -> None
    in
    SelectExpr (e, alias)

(* Parse table reference *)
let parse_table_name ps =
  let name = expect_ident ps in
  let alias =
    match peek ps with
    | Lexer.IDENT a when (let u = String.uppercase_ascii a in
                          u <> "WHERE" && u <> "ORDER" && u <> "LIMIT"
                          && u <> "ON" && u <> "JOIN" && u <> "INNER"
                          && u <> "LEFT" && u <> "RIGHT"
                          && u <> "GROUP" && u <> "HAVING") ->
      advance ps; Some a
    | _ -> None
  in
  TableName (name, alias)

let parse_join_type ps =
  match peek ps with
  | Lexer.INNER -> advance ps; expect ps Lexer.JOIN; InnerJoin
  | Lexer.LEFT -> advance ps; expect ps Lexer.JOIN; LeftJoin
  | Lexer.RIGHT -> advance ps; expect ps Lexer.JOIN; RightJoin
  | Lexer.JOIN -> advance ps; InnerJoin
  | tok -> raise (Parse_error (Printf.sprintf "expected JOIN but got %s"
    (Lexer.string_of_token tok)))

let parse_table_ref ps =
  let left = parse_table_name ps in
  let rec loop left =
    match peek ps with
    | Lexer.JOIN | Lexer.INNER | Lexer.LEFT | Lexer.RIGHT ->
      let jt = parse_join_type ps in
      let right = parse_table_name ps in
      expect ps Lexer.ON;
      let cond = parse_expr ps in
      loop (JoinClause (left, jt, right, cond))
    | _ -> left
  in
  loop left

(* Parse ORDER BY items *)
let parse_order_by_item ps =
  let e = parse_expr ps in
  let dir =
    match peek ps with
    | Lexer.ASC -> advance ps; Asc
    | Lexer.DESC -> advance ps; Desc
    | _ -> Asc
  in
  { order_expr = e; direction = dir }

(* Parse a SELECT statement *)
let parse_select ps =
  expect ps Lexer.SELECT;
  (* parse select items *)
  let first = parse_select_item ps in
  let items = ref [first] in
  while peek ps = Lexer.COMMA do
    advance ps;
    items := parse_select_item ps :: !items
  done;
  let columns = List.rev !items in
  (* FROM *)
  let from =
    if peek ps = Lexer.FROM then begin
      advance ps;
      Some (parse_table_ref ps)
    end else None
  in
  (* WHERE *)
  let where =
    if peek ps = Lexer.WHERE then begin
      advance ps;
      Some (parse_expr ps)
    end else None
  in
  (* ORDER BY *)
  let order_by =
    if peek ps = Lexer.ORDER then begin
      advance ps;
      expect ps Lexer.BY;
      let first = parse_order_by_item ps in
      let items = ref [first] in
      while peek ps = Lexer.COMMA do
        advance ps;
        items := parse_order_by_item ps :: !items
      done;
      List.rev !items
    end else []
  in
  (* LIMIT *)
  let limit =
    if peek ps = Lexer.LIMIT then begin
      advance ps;
      match peek ps with
      | Lexer.INT_LIT i -> advance ps; Some i
      | tok -> raise (Parse_error (Printf.sprintf "expected integer after LIMIT, got %s"
          (Lexer.string_of_token tok)))
    end else None
  in
  Select { columns; from; where; order_by; limit }

(* Parse a value literal for INSERT *)
let parse_value ps =
  match peek ps with
  | Lexer.INT_LIT i -> advance ps; VInt i
  | Lexer.FLOAT_LIT f -> advance ps; VFloat f
  | Lexer.STRING_LIT s -> advance ps; VString s
  | Lexer.TRUE -> advance ps; VBool true
  | Lexer.FALSE -> advance ps; VBool false
  | Lexer.NULL -> advance ps; VNull
  | Lexer.MINUS ->
    advance ps;
    (match peek ps with
     | Lexer.INT_LIT i -> advance ps; VInt (-i)
     | Lexer.FLOAT_LIT f -> advance ps; VFloat (-.f)
     | tok -> raise (Parse_error (Printf.sprintf "expected number after minus, got %s"
         (Lexer.string_of_token tok))))
  | tok -> raise (Parse_error (Printf.sprintf "expected value, got %s"
      (Lexer.string_of_token tok)))

(* Parse INSERT statement *)
let parse_insert ps =
  expect ps Lexer.INSERT;
  expect ps Lexer.INTO;
  let table = expect_ident ps in
  (* optional column list *)
  let columns =
    if peek ps = Lexer.LPAREN then begin
      advance ps;
      let first = expect_ident ps in
      let cols = ref [first] in
      while peek ps = Lexer.COMMA do
        advance ps;
        cols := expect_ident ps :: !cols
      done;
      expect ps Lexer.RPAREN;
      Some (List.rev !cols)
    end else None
  in
  expect ps Lexer.VALUES;
  (* parse value tuples *)
  let parse_value_tuple () =
    expect ps Lexer.LPAREN;
    let first = parse_value ps in
    let vals = ref [first] in
    while peek ps = Lexer.COMMA do
      advance ps;
      vals := parse_value ps :: !vals
    done;
    expect ps Lexer.RPAREN;
    List.rev !vals
  in
  let first_tuple = parse_value_tuple () in
  let tuples = ref [first_tuple] in
  while peek ps = Lexer.COMMA do
    advance ps;
    tuples := parse_value_tuple () :: !tuples
  done;
  Insert { table; columns; values = List.rev !tuples }

(* Parse data type *)
let parse_data_type ps =
  match peek ps with
  | Lexer.INT_TYPE -> advance ps; TInt
  | Lexer.FLOAT_TYPE -> advance ps; TFloat
  | Lexer.STRING_TYPE -> advance ps; TString
  | Lexer.BOOL_TYPE -> advance ps; TBool
  | tok -> raise (Parse_error (Printf.sprintf "expected data type, got %s"
      (Lexer.string_of_token tok)))

(* Parse CREATE TABLE *)
let parse_create_table ps =
  expect ps Lexer.CREATE;
  expect ps Lexer.TABLE;
  let if_not_exists =
    if peek ps = Lexer.IF then begin
      advance ps;
      expect ps Lexer.NOT;
      expect ps Lexer.EXISTS;
      true
    end else false
  in
  let table = expect_ident ps in
  expect ps Lexer.LPAREN;
  let parse_column_def () =
    let col_name = expect_ident ps in
    let col_type = parse_data_type ps in
    let nullable =
      if peek ps = Lexer.NOT then begin
        advance ps;
        expect ps Lexer.NULL;
        false
      end else true
    in
    { col_name; col_type; nullable }
  in
  let first = parse_column_def () in
  let cols = ref [first] in
  while peek ps = Lexer.COMMA do
    advance ps;
    cols := parse_column_def () :: !cols
  done;
  expect ps Lexer.RPAREN;
  CreateTable { table; columns = List.rev !cols; if_not_exists }

(* Parse DROP TABLE *)
let parse_drop_table ps =
  expect ps Lexer.DROP;
  expect ps Lexer.TABLE;
  let if_exists =
    if peek ps = Lexer.IF then begin
      advance ps;
      expect ps Lexer.EXISTS;
      true
    end else false
  in
  let table = expect_ident ps in
  DropTable { table; if_exists }

(* Main parse function *)
let parse_statement ps =
  match peek ps with
  | Lexer.SELECT -> parse_select ps
  | Lexer.INSERT -> parse_insert ps
  | Lexer.CREATE -> parse_create_table ps
  | Lexer.DROP -> parse_drop_table ps
  | tok -> raise (Parse_error (Printf.sprintf "unexpected token: %s"
      (Lexer.string_of_token tok)))

(** Parse a SQL string into an AST statement *)
let parse input =
  let tokens = Lexer.tokenize input in
  let ps = create tokens in
  let stmt = parse_statement ps in
  (* consume optional semicolon *)
  if peek ps = Lexer.SEMICOLON then advance ps;
  (* ensure we consumed everything *)
  if peek ps <> Lexer.EOF then
    raise (Parse_error (Printf.sprintf "unexpected token after statement: %s"
      (Lexer.string_of_token (peek ps))));
  stmt
