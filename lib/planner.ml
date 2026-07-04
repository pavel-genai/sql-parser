(** Query planner: converts AST to physical execution plan *)

open Ast

(** Physical plan nodes *)
type plan =
  | Scan of string  (* table name *)
  | Filter of plan * expr
  | Project of plan * select_item list
  | Sort of plan * order_item list
  | Limit of plan * int
  | Join of plan * plan * join_type * expr
  | InsertPlan of string * string list option * value list list
  | CreateTablePlan of string * column_def list * bool
  | DropTablePlan of string * bool
  | Empty  (* for SELECT without FROM *)

(** Convert a table_ref to a plan *)
let rec plan_from_table_ref = function
  | TableName (name, _alias) -> Scan name
  | JoinClause (left, jt, right, cond) ->
    let lp = plan_from_table_ref left in
    let rp = plan_from_table_ref right in
    Join (lp, rp, jt, cond)

(** Build a physical plan from an AST statement *)
let plan_statement stmt =
  match stmt with
  | Select { columns; from; where; order_by; limit } ->
    let base = match from with
      | Some tr -> plan_from_table_ref tr
      | None -> Empty
    in
    let filtered = match where with
      | Some expr -> Filter (base, expr)
      | None -> base
    in
    let sorted = match order_by with
      | [] -> filtered
      | items -> Sort (filtered, items)
    in
    let limited = match limit with
      | Some n -> Limit (sorted, n)
      | None -> sorted
    in
    (* Project is always the outermost for SELECT *)
    Project (limited, columns)
  | Insert { table; columns; values } ->
    InsertPlan (table, columns, values)
  | CreateTable { table; columns; if_not_exists } ->
    CreateTablePlan (table, columns, if_not_exists)
  | DropTable { table; if_exists } ->
    DropTablePlan (table, if_exists)

(** Pretty-print a plan for debugging *)
let rec string_of_plan indent = function
  | Scan name -> indent ^ "Scan(" ^ name ^ ")"
  | Filter (child, _) ->
    indent ^ "Filter\n" ^ string_of_plan (indent ^ "  ") child
  | Project (child, _) ->
    indent ^ "Project\n" ^ string_of_plan (indent ^ "  ") child
  | Sort (child, _) ->
    indent ^ "Sort\n" ^ string_of_plan (indent ^ "  ") child
  | Limit (child, n) ->
    indent ^ "Limit(" ^ string_of_int n ^ ")\n" ^ string_of_plan (indent ^ "  ") child
  | Join (left, right, _, _) ->
    indent ^ "Join\n" ^
    string_of_plan (indent ^ "  ") left ^ "\n" ^
    string_of_plan (indent ^ "  ") right
  | InsertPlan (table, _, _) -> indent ^ "Insert(" ^ table ^ ")"
  | CreateTablePlan (table, _, _) -> indent ^ "CreateTable(" ^ table ^ ")"
  | DropTablePlan (table, _) -> indent ^ "DropTable(" ^ table ^ ")"
  | Empty -> indent ^ "Empty"
