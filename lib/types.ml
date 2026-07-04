(** Type checker for SQL AST: validates column references and types *)

open Ast

(** Schema: mapping of table name to column definitions *)
type table_schema = {
  schema_table : string;
  schema_columns : column_def list;
}

type schema = table_schema list

exception Type_error of string

(** Find a table schema by name *)
let find_table schema name =
  match List.find_opt (fun ts -> String.lowercase_ascii ts.schema_table = String.lowercase_ascii name) schema with
  | Some ts -> ts
  | None -> raise (Type_error (Printf.sprintf "unknown table: %s" name))

(** Get the table name from a table_ref *)
let rec table_names_from_ref = function
  | TableName (name, alias) -> [(name, alias)]
  | JoinClause (left, _, right, _) ->
    table_names_from_ref left @ table_names_from_ref right

(** Resolve a column reference against the schema and available tables *)
let resolve_column schema tables col_ref =
  let check_table tname =
    let ts = find_table schema tname in
    List.find_opt (fun cd ->
      String.lowercase_ascii cd.col_name = String.lowercase_ascii col_ref.column
    ) ts.schema_columns
  in
  match col_ref.table with
  | Some tname ->
    (* Check both actual table names and aliases *)
    let actual_name =
      match List.find_opt (fun (name, alias) ->
        String.lowercase_ascii name = String.lowercase_ascii tname ||
        (match alias with Some a -> String.lowercase_ascii a = String.lowercase_ascii tname | None -> false)
      ) tables with
      | Some (name, _) -> name
      | None -> raise (Type_error (Printf.sprintf "unknown table or alias: %s" tname))
    in
    (match check_table actual_name with
     | Some cd -> cd.col_type
     | None -> raise (Type_error (Printf.sprintf "unknown column %s in table %s" col_ref.column actual_name)))
  | None ->
    (* Search all available tables *)
    let found = List.filter_map (fun (tname, _) ->
      match check_table tname with
      | Some cd -> Some (tname, cd.col_type)
      | None -> None
    ) tables in
    (match found with
     | [] -> raise (Type_error (Printf.sprintf "unknown column: %s" col_ref.column))
     | [(_,t)] -> t
     | _ -> raise (Type_error (Printf.sprintf "ambiguous column reference: %s" col_ref.column)))

(** Type-check an expression and return its result type *)
let rec check_expr schema tables expr =
  match expr with
  | Literal (VInt _) -> TInt
  | Literal (VFloat _) -> TFloat
  | Literal (VString _) -> TString
  | Literal (VBool _) -> TBool
  | Literal VNull -> TString (* NULL is polymorphic, default to string *)
  | Column col_ref -> resolve_column schema tables col_ref
  | BinOp (op, left, right) ->
    let _lt = check_expr schema tables left in
    let _rt = check_expr schema tables right in
    (match op with
     | Eq | Neq | Lt | Gt | Lte | Gte -> TBool
     | And | Or -> TBool
     | Add | Sub | Mul | Div -> _lt) (* keep the left type *)
  | UnOp (Not, e) ->
    let _ = check_expr schema tables e in
    TBool
  | UnOp (Neg, e) ->
    check_expr schema tables e
  | FuncCall (_, args) ->
    (* Simple: just check args, return string *)
    List.iter (fun a -> ignore (check_expr schema tables a)) args;
    TString

(** Type-check a full statement *)
let check_statement schema stmt =
  match stmt with
  | Select { columns; from; where; order_by; _ } ->
    let tables = match from with
      | Some tr -> table_names_from_ref tr
      | None -> []
    in
    (* Check column references in select items *)
    List.iter (fun item ->
      match item with
      | SelectExpr (e, _) -> ignore (check_expr schema tables e)
      | SelectStar -> ()
      | SelectTableStar tname ->
        ignore (find_table schema
          (match List.find_opt (fun (name, alias) ->
            String.lowercase_ascii name = String.lowercase_ascii tname ||
            (match alias with Some a -> String.lowercase_ascii a = String.lowercase_ascii tname | None -> false)
          ) tables with
          | Some (name, _) -> name
          | None -> tname))
    ) columns;
    (* Check WHERE *)
    (match where with
     | Some e -> ignore (check_expr schema tables e)
     | None -> ());
    (* Check ORDER BY *)
    List.iter (fun oi -> ignore (check_expr schema tables oi.order_expr)) order_by;
    (* Check JOIN conditions *)
    let rec check_join_conds = function
      | TableName _ -> ()
      | JoinClause (l, _, r, cond) ->
        check_join_conds l;
        check_join_conds r;
        ignore (check_expr schema tables cond)
    in
    (match from with Some tr -> check_join_conds tr | None -> ())
  | Insert { table; columns; values } ->
    let ts = find_table schema table in
    let expected_cols = match columns with
      | Some cols -> List.length cols
      | None -> List.length ts.schema_columns
    in
    List.iter (fun row ->
      if List.length row <> expected_cols then
        raise (Type_error (Printf.sprintf
          "INSERT into %s: expected %d values but got %d"
          table expected_cols (List.length row)))
    ) values
  | CreateTable { columns; _ } ->
    (* Check for duplicate column names *)
    let names = List.map (fun cd -> String.lowercase_ascii cd.col_name) columns in
    let rec check_dup = function
      | [] -> ()
      | x :: rest ->
        if List.mem x rest then
          raise (Type_error (Printf.sprintf "duplicate column name: %s" x));
        check_dup rest
    in
    check_dup names
  | DropTable _ -> ()
