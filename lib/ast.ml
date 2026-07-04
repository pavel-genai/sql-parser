(** SQL Abstract Syntax Tree definitions *)

(** Data types supported by the SQL subset *)
type data_type =
  | TInt
  | TFloat
  | TString
  | TBool

(** Runtime values *)
type value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
  | VNull

(** Binary operators *)
type binop =
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  | Add
  | Sub
  | Mul
  | Div

(** Unary operators *)
type unop =
  | Not
  | Neg

(** Sort direction *)
type direction =
  | Asc
  | Desc

(** Expressions *)
type expr =
  | Literal of value
  | Column of column_ref
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | FuncCall of string * expr list

(** Column reference: optional table qualifier and column name *)
and column_ref = {
  table : string option;
  column : string;
}

(** Select item: expression with optional alias *)
type select_item =
  | SelectExpr of expr * string option
  | SelectStar
  | SelectTableStar of string

(** Join type *)
type join_type =
  | InnerJoin
  | LeftJoin
  | RightJoin

(** Table reference *)
type table_ref =
  | TableName of string * string option  (* name, optional alias *)
  | JoinClause of table_ref * join_type * table_ref * expr

(** ORDER BY clause *)
type order_item = {
  order_expr : expr;
  direction : direction;
}

(** Column definition for CREATE TABLE *)
type column_def = {
  col_name : string;
  col_type : data_type;
  nullable : bool;
}

(** SQL statements *)
type statement =
  | Select of {
      columns : select_item list;
      from : table_ref option;
      where : expr option;
      order_by : order_item list;
      limit : int option;
    }
  | Insert of {
      table : string;
      columns : string list option;
      values : value list list;
    }
  | CreateTable of {
      table : string;
      columns : column_def list;
      if_not_exists : bool;
    }
  | DropTable of {
      table : string;
      if_exists : bool;
    }

(** Pretty-print a value *)
let string_of_value = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VString s -> s
  | VBool b -> string_of_bool b
  | VNull -> "NULL"

(** Pretty-print a data type *)
let string_of_data_type = function
  | TInt -> "INT"
  | TFloat -> "FLOAT"
  | TString -> "STRING"
  | TBool -> "BOOL"
