(** CSV-backed storage engine *)

open Ast

exception Storage_error of string

(** Represents a table stored as a CSV file *)
type table = {
  name : string;
  columns : column_def list;
  mutable rows : value list list;
}

(** The database: a collection of tables backed by CSV files *)
type database = {
  data_dir : string;
  mutable tables : table list;
}

(** Create a database with a given data directory *)
let create_database data_dir =
  (try Unix.mkdir data_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  { data_dir; tables = [] }

(** CSV file path for a table *)
let table_path db name =
  Filename.concat db.data_dir (String.lowercase_ascii name ^ ".csv")

(** Schema file path for a table *)
let schema_path db name =
  Filename.concat db.data_dir (String.lowercase_ascii name ^ ".schema")

(** Parse a CSV line, handling quoted strings *)
let parse_csv_line line =
  let len = String.length line in
  if len = 0 then []
  else begin
    let fields = ref [] in
    let buf = Buffer.create 64 in
    let in_quotes = ref false in
    let i = ref 0 in
    while !i < len do
      let c = line.[!i] in
      if !in_quotes then begin
        if c = '"' then begin
          if !i + 1 < len && line.[!i + 1] = '"' then begin
            Buffer.add_char buf '"';
            i := !i + 1
          end else
            in_quotes := false
        end else
          Buffer.add_char buf c
      end else begin
        if c = '"' then
          in_quotes := true
        else if c = ',' then begin
          fields := Buffer.contents buf :: !fields;
          Buffer.clear buf
        end else
          Buffer.add_char buf c
      end;
      i := !i + 1
    done;
    fields := Buffer.contents buf :: !fields;
    List.rev !fields
  end

(** Serialize a value to CSV field *)
let value_to_csv = function
  | VInt i -> string_of_int i
  | VFloat f -> Printf.sprintf "%.17g" f
  | VString s ->
    if String.contains s ',' || String.contains s '"' || String.contains s '\n' then
      "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
    else s
  | VBool b -> if b then "true" else "false"
  | VNull -> ""

(** Parse a CSV field to a value given the expected type *)
let csv_to_value col_type field =
  let trimmed = String.trim field in
  if trimmed = "" then VNull
  else match col_type with
    | TInt -> (try VInt (int_of_string trimmed) with _ -> VString trimmed)
    | TFloat -> (try VFloat (float_of_string trimmed) with _ -> VString trimmed)
    | TBool ->
      (match String.lowercase_ascii trimmed with
       | "true" | "1" -> VBool true
       | "false" | "0" -> VBool false
       | _ -> VString trimmed)
    | TString -> VString trimmed

(** Save a table schema to disk *)
let save_schema db tbl =
  let path = schema_path db tbl.name in
  let oc = open_out path in
  List.iter (fun col ->
    Printf.fprintf oc "%s,%s,%s\n"
      col.col_name
      (string_of_data_type col.col_type)
      (if col.nullable then "NULL" else "NOT NULL")
  ) tbl.columns;
  close_out oc

(** Load a table schema from disk *)
let load_schema db name =
  let path = schema_path db name in
  if not (Sys.file_exists path) then
    raise (Storage_error (Printf.sprintf "schema file not found for table %s" name));
  let ic = open_in path in
  let cols = ref [] in
  (try while true do
    let line = input_line ic in
    match parse_csv_line line with
    | [col_name; type_str; null_str] ->
      let col_type = match String.uppercase_ascii type_str with
        | "INT" -> TInt
        | "FLOAT" -> TFloat
        | "STRING" -> TString
        | "BOOL" -> TBool
        | _ -> TString
      in
      let nullable = null_str <> "NOT NULL" in
      cols := { col_name; col_type; nullable } :: !cols
    | _ -> ()
  done with End_of_file -> ());
  close_in ic;
  List.rev !cols

(** Save table data to CSV *)
let save_data db tbl =
  let path = table_path db tbl.name in
  let oc = open_out path in
  (* Write header *)
  let header = List.map (fun col -> col.col_name) tbl.columns in
  Printf.fprintf oc "%s\n" (String.concat "," header);
  (* Write rows *)
  List.iter (fun row ->
    let fields = List.map value_to_csv row in
    Printf.fprintf oc "%s\n" (String.concat "," fields)
  ) tbl.rows;
  close_out oc

(** Load table data from CSV *)
let load_data db name columns =
  let path = table_path db name in
  if not (Sys.file_exists path) then []
  else begin
    let ic = open_in path in
    let rows = ref [] in
    let is_header = ref true in
    (try while true do
      let line = input_line ic in
      if !is_header then
        is_header := false
      else begin
        let fields = parse_csv_line line in
        let row = List.mapi (fun i field ->
          if i < List.length columns then
            csv_to_value (List.nth columns i).col_type field
          else VString field
        ) fields in
        rows := row :: !rows
      end
    done with End_of_file -> ());
    close_in ic;
    List.rev !rows
  end

(** Create a new table *)
let create_table db name columns ~if_not_exists =
  if List.exists (fun t -> String.lowercase_ascii t.name = String.lowercase_ascii name) db.tables then begin
    if if_not_exists then ()
    else raise (Storage_error (Printf.sprintf "table %s already exists" name))
  end else begin
    let tbl = { name; columns; rows = [] } in
    db.tables <- tbl :: db.tables;
    save_schema db tbl;
    save_data db tbl
  end

(** Drop a table *)
let drop_table db name ~if_exists =
  let lname = String.lowercase_ascii name in
  if not (List.exists (fun t -> String.lowercase_ascii t.name = lname) db.tables) then begin
    if if_exists then ()
    else raise (Storage_error (Printf.sprintf "table %s does not exist" name))
  end else begin
    db.tables <- List.filter (fun t -> String.lowercase_ascii t.name <> lname) db.tables;
    let csv = table_path db name in
    let sch = schema_path db name in
    (try Sys.remove csv with Sys_error _ -> ());
    (try Sys.remove sch with Sys_error _ -> ())
  end

(** Get a table, loading from disk if necessary *)
let get_table db name =
  let lname = String.lowercase_ascii name in
  match List.find_opt (fun t -> String.lowercase_ascii t.name = lname) db.tables with
  | Some tbl -> tbl
  | None ->
    (* Try loading from disk *)
    let sch_path = schema_path db name in
    if Sys.file_exists sch_path then begin
      let columns = load_schema db name in
      let rows = load_data db name columns in
      let tbl = { name; columns; rows } in
      db.tables <- tbl :: db.tables;
      tbl
    end else
      raise (Storage_error (Printf.sprintf "table %s does not exist" name))

(** Insert rows into a table *)
let insert_rows db name columns values =
  let tbl = get_table db name in
  let ordered_values =
    match columns with
    | None -> values
    | Some col_names ->
      (* Reorder values to match table schema *)
      List.map (fun row ->
        List.map (fun schema_col ->
          let idx = ref (-1) in
          List.iteri (fun i cn ->
            if String.lowercase_ascii cn = String.lowercase_ascii schema_col.col_name then
              idx := i
          ) col_names;
          if !idx >= 0 then List.nth row !idx
          else VNull
        ) tbl.columns
      ) values
  in
  tbl.rows <- tbl.rows @ ordered_values;
  save_data db tbl;
  List.length ordered_values

(** Get schema information for type checking *)
let get_schema db : Types.table_schema list =
  List.map (fun tbl ->
    { Types.schema_table = tbl.name; schema_columns = tbl.columns }
  ) db.tables

(** Scan all rows of a table *)
let scan_table db name =
  let tbl = get_table db name in
  (tbl.columns, tbl.rows)
