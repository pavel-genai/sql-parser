(** SQL Lexer / Tokenizer *)

(** Token types *)
type token =
  (* Keywords *)
  | SELECT | FROM | WHERE | INSERT | INTO | VALUES
  | CREATE | TABLE | DROP | ORDER | BY | ASC | DESC
  | LIMIT | JOIN | INNER | LEFT | RIGHT | ON
  | AND | OR | NOT | NULL | TRUE | FALSE
  | INT_TYPE | FLOAT_TYPE | STRING_TYPE | BOOL_TYPE
  | IF | EXISTS
  (* Literals *)
  | INT_LIT of int
  | FLOAT_LIT of float
  | STRING_LIT of string
  | IDENT of string
  (* Operators *)
  | STAR | COMMA | DOT | SEMICOLON
  | LPAREN | RPAREN
  | EQ | NEQ | LT | GT | LTE | GTE
  | PLUS | MINUS | SLASH
  (* Special *)
  | EOF

type lexer_state = {
  input : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
}

exception Lexer_error of string

let create input = { input; pos = 0; line = 1; col = 1 }

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c
let is_whitespace c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

let peek st =
  if st.pos < String.length st.input then Some st.input.[st.pos]
  else None

let advance st =
  if st.pos < String.length st.input then begin
    if st.input.[st.pos] = '\n' then begin
      st.line <- st.line + 1;
      st.col <- 1
    end else
      st.col <- st.col + 1;
    st.pos <- st.pos + 1
  end

let current st =
  if st.pos < String.length st.input then st.input.[st.pos]
  else raise (Lexer_error "unexpected end of input")

let skip_whitespace st =
  while st.pos < String.length st.input && is_whitespace st.input.[st.pos] do
    advance st
  done

let skip_line_comment st =
  (* skip -- comments *)
  while st.pos < String.length st.input && st.input.[st.pos] <> '\n' do
    advance st
  done

let read_string st =
  let quote = current st in
  advance st; (* skip opening quote *)
  let buf = Buffer.create 64 in
  while st.pos < String.length st.input && st.input.[st.pos] <> quote do
    if st.input.[st.pos] = '\\' then begin
      advance st;
      if st.pos < String.length st.input then begin
        let c = match st.input.[st.pos] with
          | 'n' -> '\n'
          | 't' -> '\t'
          | '\\' -> '\\'
          | c when c = quote -> quote
          | c -> c
        in
        Buffer.add_char buf c;
        advance st
      end
    end else begin
      Buffer.add_char buf (st.input.[st.pos]);
      advance st
    end
  done;
  if st.pos >= String.length st.input then
    raise (Lexer_error "unterminated string literal");
  advance st; (* skip closing quote *)
  Buffer.contents buf

let read_number st =
  let buf = Buffer.create 16 in
  let is_float = ref false in
  while st.pos < String.length st.input && (is_digit st.input.[st.pos] || st.input.[st.pos] = '.') do
    if st.input.[st.pos] = '.' then is_float := true;
    Buffer.add_char buf st.input.[st.pos];
    advance st
  done;
  let s = Buffer.contents buf in
  if !is_float then FLOAT_LIT (float_of_string s)
  else INT_LIT (int_of_string s)

let read_ident st =
  let buf = Buffer.create 16 in
  while st.pos < String.length st.input && is_alnum st.input.[st.pos] do
    Buffer.add_char buf st.input.[st.pos];
    advance st
  done;
  let word = Buffer.contents buf in
  match String.uppercase_ascii word with
  | "SELECT" -> SELECT
  | "FROM" -> FROM
  | "WHERE" -> WHERE
  | "INSERT" -> INSERT
  | "INTO" -> INTO
  | "VALUES" -> VALUES
  | "CREATE" -> CREATE
  | "TABLE" -> TABLE
  | "DROP" -> DROP
  | "ORDER" -> ORDER
  | "BY" -> BY
  | "ASC" -> ASC
  | "DESC" -> DESC
  | "LIMIT" -> LIMIT
  | "JOIN" -> JOIN
  | "INNER" -> INNER
  | "LEFT" -> LEFT
  | "RIGHT" -> RIGHT
  | "ON" -> ON
  | "AND" -> AND
  | "OR" -> OR
  | "NOT" -> NOT
  | "NULL" -> NULL
  | "TRUE" -> TRUE
  | "FALSE" -> FALSE
  | "INT" | "INTEGER" -> INT_TYPE
  | "FLOAT" | "REAL" | "DOUBLE" -> FLOAT_TYPE
  | "STRING" | "TEXT" | "VARCHAR" -> STRING_TYPE
  | "BOOL" | "BOOLEAN" -> BOOL_TYPE
  | "IF" -> IF
  | "EXISTS" -> EXISTS
  | _ -> IDENT word

let rec next_token st =
  skip_whitespace st;
  match peek st with
  | None -> EOF
  | Some c ->
    match c with
    | '-' ->
      advance st;
      (match peek st with
       | Some '-' -> advance st; skip_line_comment st; next_token st
       | _ -> MINUS)
    | '\'' | '"' -> STRING_LIT (read_string st)
    | c when is_digit c -> read_number st
    | c when is_alpha c -> read_ident st
    | '*' -> advance st; STAR
    | ',' -> advance st; COMMA
    | '.' -> advance st; DOT
    | ';' -> advance st; SEMICOLON
    | '(' -> advance st; LPAREN
    | ')' -> advance st; RPAREN
    | '=' -> advance st; EQ
    | '<' ->
      advance st;
      (match peek st with
       | Some '>' -> advance st; NEQ
       | Some '=' -> advance st; LTE
       | _ -> LT)
    | '>' ->
      advance st;
      (match peek st with
       | Some '=' -> advance st; GTE
       | _ -> GT)
    | '!' ->
      advance st;
      (match peek st with
       | Some '=' -> advance st; NEQ
       | _ -> raise (Lexer_error (Printf.sprintf "unexpected character '!' at line %d, col %d" st.line st.col)))
    | '+' -> advance st; PLUS
    | '/' -> advance st; SLASH
    | c -> raise (Lexer_error (Printf.sprintf "unexpected character '%c' at line %d, col %d" c st.line st.col))

(** Tokenize an entire input string *)
let tokenize input =
  let st = create input in
  let tokens = ref [] in
  let rec loop () =
    let tok = next_token st in
    tokens := tok :: !tokens;
    if tok <> EOF then loop ()
  in
  loop ();
  List.rev !tokens

(** String representation of a token (for debugging) *)
let string_of_token = function
  | SELECT -> "SELECT" | FROM -> "FROM" | WHERE -> "WHERE"
  | INSERT -> "INSERT" | INTO -> "INTO" | VALUES -> "VALUES"
  | CREATE -> "CREATE" | TABLE -> "TABLE" | DROP -> "DROP"
  | ORDER -> "ORDER" | BY -> "BY" | ASC -> "ASC" | DESC -> "DESC"
  | LIMIT -> "LIMIT" | JOIN -> "JOIN" | INNER -> "INNER"
  | LEFT -> "LEFT" | RIGHT -> "RIGHT" | ON -> "ON"
  | AND -> "AND" | OR -> "OR" | NOT -> "NOT" | NULL -> "NULL"
  | TRUE -> "TRUE" | FALSE -> "FALSE"
  | INT_TYPE -> "INT" | FLOAT_TYPE -> "FLOAT"
  | STRING_TYPE -> "STRING" | BOOL_TYPE -> "BOOL"
  | IF -> "IF" | EXISTS -> "EXISTS"
  | INT_LIT i -> Printf.sprintf "INT(%d)" i
  | FLOAT_LIT f -> Printf.sprintf "FLOAT(%f)" f
  | STRING_LIT s -> Printf.sprintf "STRING('%s')" s
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | STAR -> "*" | COMMA -> "," | DOT -> "." | SEMICOLON -> ";"
  | LPAREN -> "(" | RPAREN -> ")"
  | EQ -> "=" | NEQ -> "<>" | LT -> "<" | GT -> ">"
  | LTE -> "<=" | GTE -> ">="
  | PLUS -> "+" | MINUS -> "-" | SLASH -> "/"
  | EOF -> "EOF"
