(** Query executor: executes physical plans against the storage engine *)

open Ast

(** Query result *)
type result = {
  columns : string list;
  rows : value list list;
  message : string option;
}

exception Exec_error of string

(** Evaluate an expression against a row with named columns *)
let rec eval_expr col_names row expr =
  match expr with
  | Literal v -> v
  | Column { table = _; column = name } ->
    let lname = String.lowercase_ascii name in
    let rec find cols vals =
      match cols, vals with
      | [], _ | _, [] -> VNull
      | c :: _, v :: _ when String.lowercase_ascii c = lname -> v
      | _ :: cs, _ :: vs -> find cs vs
    in
    find col_names row
  | BinOp (op, left, right) ->
    let lv = eval_expr col_names row left in
    let rv = eval_expr col_names row right in
    eval_binop op lv rv
  | UnOp (Not, e) ->
    (match eval_expr col_names row e with
     | VBool b -> VBool (not b)
     | VNull -> VNull
     | _ -> raise (Exec_error "NOT requires boolean"))
  | UnOp (Neg, e) ->
    (match eval_expr col_names row e with
     | VInt i -> VInt (-i)
     | VFloat f -> VFloat (-.f)
     | VNull -> VNull
     | _ -> raise (Exec_error "unary minus requires number"))
  | FuncCall (name, args) ->
    let arg_vals = List.map (eval_expr col_names row) args in
    eval_func name arg_vals

and eval_binop op lv rv =
  match op with
  | Eq -> VBool (compare_values lv rv = 0)
  | Neq -> VBool (compare_values lv rv <> 0)
  | Lt -> VBool (compare_values lv rv < 0)
  | Gt -> VBool (compare_values lv rv > 0)
  | Lte -> VBool (compare_values lv rv <= 0)
  | Gte -> VBool (compare_values lv rv >= 0)
  | And ->
    (match lv, rv with
     | VBool a, VBool b -> VBool (a && b)
     | VNull, _ | _, VNull -> VNull
     | _ -> raise (Exec_error "AND requires booleans"))
  | Or ->
    (match lv, rv with
     | VBool a, VBool b -> VBool (a || b)
     | VNull, _ | _, VNull -> VNull
     | _ -> raise (Exec_error "OR requires booleans"))
  | Add -> numeric_op ( + ) ( +. ) lv rv
  | Sub -> numeric_op ( - ) ( -. ) lv rv
  | Mul -> numeric_op ( * ) ( *. ) lv rv
  | Div ->
    (match lv, rv with
     | VInt _, VInt 0 -> raise (Exec_error "division by zero")
     | VFloat _, VFloat 0.0 -> raise (Exec_error "division by zero")
     | _ -> numeric_op ( / ) ( /. ) lv rv)

and compare_values a b =
  match a, b with
  | VNull, VNull -> 0
  | VNull, _ -> -1
  | _, VNull -> 1
  | VInt a, VInt b -> compare a b
  | VFloat a, VFloat b -> compare a b
  | VInt a, VFloat b -> compare (float_of_int a) b
  | VFloat a, VInt b -> compare a (float_of_int b)
  | VString a, VString b -> String.compare a b
  | VBool a, VBool b -> compare a b
  | a, b -> String.compare (string_of_value a) (string_of_value b)

and numeric_op iop fop lv rv =
  match lv, rv with
  | VInt a, VInt b -> VInt (iop a b)
  | VFloat a, VFloat b -> VFloat (fop a b)
  | VInt a, VFloat b -> VFloat (fop (float_of_int a) b)
  | VFloat a, VInt b -> VFloat (fop a (float_of_int b))
  | VNull, _ | _, VNull -> VNull
  | _ -> raise (Exec_error "arithmetic requires numbers")

and eval_func name args =
  match String.uppercase_ascii name, args with
  | "UPPER", [VString s] -> VString (String.uppercase_ascii s)
  | "LOWER", [VString s] -> VString (String.lowercase_ascii s)
  | "LENGTH", [VString s] -> VInt (String.length s)
  | "CONCAT", vals ->
    VString (String.concat "" (List.map string_of_value vals))
  | _ -> raise (Exec_error (Printf.sprintf "unknown function: %s" name))

(** Check if value is truthy *)
let is_truthy = function
  | VBool true -> true
  | VInt n -> n <> 0
  | VFloat f -> f <> 0.0
  | VString s -> s <> ""
  | VBool false | VNull -> false

(** Execute a project operation on a single row *)
let project_row all_col_names row items =
  let project_item = function
    | SelectStar -> (all_col_names, row)
    | SelectTableStar _tname ->
      (* For simplicity, return all columns (join would need qualified names) *)
      (all_col_names, row)
    | SelectExpr (expr, alias) ->
      let v = eval_expr all_col_names row expr in
      let name = match alias with
        | Some a -> a
        | None ->
          (match expr with
           | Column { column; _ } -> column
           | _ -> "?column?")
      in
      ([name], [v])
  in
  let parts = List.map project_item items in
  let names = List.concat_map fst parts in
  let vals = List.concat_map snd parts in
  (names, vals)

(** Execute a physical plan *)
let rec execute db plan =
  match plan with
  | Planner.Scan table_name ->
    let (col_defs, rows) = Storage.scan_table db table_name in
    let col_names = List.map (fun cd -> cd.col_name) col_defs in
    { columns = col_names; rows; message = None }

  | Planner.Empty ->
    { columns = []; rows = [[]]; message = None }

  | Planner.Filter (child, expr) ->
    let result = execute db child in
    let filtered = List.filter (fun row ->
      is_truthy (eval_expr result.columns row expr)
    ) result.rows in
    { result with rows = filtered }

  | Planner.Project (child, items) ->
    let result = execute db child in
    if result.rows = [] then begin
      (* Still need to figure out column names *)
      let dummy_row = List.map (fun _ -> VNull) result.columns in
      let (names, _) = project_row result.columns dummy_row items in
      { columns = names; rows = []; message = None }
    end else begin
      let projected = List.map (fun row ->
        project_row result.columns row items
      ) result.rows in
      let names = match projected with
        | (n, _) :: _ -> n
        | [] -> []
      in
      let rows = List.map snd projected in
      { columns = names; rows; message = None }
    end

  | Planner.Sort (child, order_items) ->
    let result = execute db child in
    let sorted = List.sort (fun row_a row_b ->
      let rec cmp = function
        | [] -> 0
        | oi :: rest ->
          let va = eval_expr result.columns row_a oi.order_expr in
          let vb = eval_expr result.columns row_b oi.order_expr in
          let c = compare_values va vb in
          let c = match oi.direction with Asc -> c | Desc -> -c in
          if c <> 0 then c else cmp rest
      in
      cmp order_items
    ) result.rows in
    { result with rows = sorted }

  | Planner.Limit (child, n) ->
    let result = execute db child in
    let limited = List.filteri (fun i _ -> i < n) result.rows in
    { result with rows = limited }

  | Planner.Join (left_plan, right_plan, join_type, cond) ->
    let left = execute db left_plan in
    let right = execute db right_plan in
    let all_cols = left.columns @ right.columns in
    let joined_rows = ref [] in
    List.iter (fun lrow ->
      let matched = ref false in
      List.iter (fun rrow ->
        let combined = lrow @ rrow in
        if is_truthy (eval_expr all_cols combined cond) then begin
          joined_rows := combined :: !joined_rows;
          matched := true
        end
      ) right.rows;
      if not !matched then
        match join_type with
        | LeftJoin ->
          let nulls = List.map (fun _ -> VNull) right.columns in
          joined_rows := (lrow @ nulls) :: !joined_rows
        | _ -> ()
    ) left.rows;
    (* Handle RIGHT JOIN *)
    (match join_type with
     | RightJoin ->
       List.iter (fun rrow ->
         let matched = List.exists (fun lrow ->
           let combined = lrow @ rrow in
           is_truthy (eval_expr all_cols combined cond)
         ) left.rows in
         if not matched then begin
           let nulls = List.map (fun _ -> VNull) left.columns in
           joined_rows := (nulls @ rrow) :: !joined_rows
         end
       ) right.rows
     | _ -> ());
    { columns = all_cols; rows = List.rev !joined_rows; message = None }

  | Planner.InsertPlan (table, columns, values) ->
    let count = Storage.insert_rows db table columns values in
    { columns = []; rows = [];
      message = Some (Printf.sprintf "Inserted %d row(s)" count) }

  | Planner.CreateTablePlan (table, columns, if_not_exists) ->
    Storage.create_table db table columns ~if_not_exists;
    { columns = []; rows = [];
      message = Some (Printf.sprintf "Table %s created" table) }

  | Planner.DropTablePlan (table, if_exists) ->
    Storage.drop_table db table ~if_exists;
    { columns = []; rows = [];
      message = Some (Printf.sprintf "Table %s dropped" table) }

(** Format a result for display *)
let format_result result =
  match result.message with
  | Some msg -> msg
  | None ->
    if result.columns = [] && result.rows = [] then "OK"
    else begin
      let buf = Buffer.create 256 in
      (* Calculate column widths *)
      let ncols = List.length result.columns in
      let widths = Array.make ncols 0 in
      List.iteri (fun i name ->
        widths.(i) <- String.length name
      ) result.columns;
      List.iter (fun row ->
        List.iteri (fun i v ->
          if i < ncols then
            widths.(i) <- max widths.(i) (String.length (string_of_value v))
        ) row
      ) result.rows;
      (* Header *)
      let sep = String.concat "+" (List.init ncols (fun i ->
        String.make (widths.(i) + 2) '-'
      )) in
      Buffer.add_string buf ("+" ^ sep ^ "+\n");
      let header = String.concat "|" (List.mapi (fun i name ->
        let pad = widths.(i) - String.length name in
        " " ^ name ^ String.make (pad + 1) ' '
      ) result.columns) in
      Buffer.add_string buf ("|" ^ header ^ "|\n");
      Buffer.add_string buf ("+" ^ sep ^ "+\n");
      (* Rows *)
      List.iter (fun row ->
        let line = String.concat "|" (List.mapi (fun i v ->
          let s = string_of_value v in
          let pad = (if i < ncols then widths.(i) else String.length s) - String.length s in
          " " ^ s ^ String.make (pad + 1) ' '
        ) row) in
        Buffer.add_string buf ("|" ^ line ^ "|\n")
      ) result.rows;
      Buffer.add_string buf ("+" ^ sep ^ "+\n");
      Buffer.add_string buf (Printf.sprintf "%d row(s)\n" (List.length result.rows));
      Buffer.contents buf
    end
