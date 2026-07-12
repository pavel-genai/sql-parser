(** SQL Parser HTTP server.

    Dependency-free HTTP/1.1 server built on the Unix module. Serves:
    - GET  /health : liveness probe
    - POST /query  : execute a SQL statement (plain-text body) against a
      process-lifetime database instance, so state persists across requests. *)

let default_port = 8080

let port =
  match Sys.getenv_opt "PORT" with
  | Some p -> (try int_of_string p with _ -> default_port)
  | None -> default_port

(* Single database instance shared by all requests for the lifetime of the
   process, mirroring the REPL in main.ml. *)
let db = Sql_parser.Storage.create_database "data"

(* ------------------------------------------------------------------ *)
(* JSON serialization                                                  *)
(* ------------------------------------------------------------------ *)

let json_escape s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
       match c with
       | '"' -> Buffer.add_string buf "\\\""
       | '\\' -> Buffer.add_string buf "\\\\"
       | '\n' -> Buffer.add_string buf "\\n"
       | '\r' -> Buffer.add_string buf "\\r"
       | '\t' -> Buffer.add_string buf "\\t"
       | c when Char.code c < 0x20 ->
         Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
       | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let json_of_value (v : Sql_parser.Ast.value) =
  match v with
  | VString s -> "\"" ^ json_escape s ^ "\""
  | VNull -> "null"
  | VFloat f when not (Float.is_finite f) -> "null"
  | VFloat f -> Printf.sprintf "%.17g" f
  (* VInt / VBool print as valid JSON literals already *)
  | (VInt _ | VBool _) as v -> Sql_parser.Ast.string_of_value v

let json_of_result (r : Sql_parser.Executor.result) =
  match r.message with
  | Some msg ->
    Printf.sprintf "{\"status\":\"ok\",\"message\":\"%s\"}" (json_escape msg)
  | None ->
    let columns =
      String.concat ","
        (List.map (fun c -> "\"" ^ json_escape c ^ "\"") r.columns)
    in
    let rows =
      String.concat ","
        (List.map
           (fun row ->
              "[" ^ String.concat "," (List.map json_of_value row) ^ "]")
           r.rows)
    in
    Printf.sprintf
      "{\"status\":\"ok\",\"columns\":[%s],\"rows\":[%s],\"row_count\":%d}"
      columns rows (List.length r.rows)

let json_error msg =
  Printf.sprintf "{\"status\":\"error\",\"error\":\"%s\"}" (json_escape msg)

(* ------------------------------------------------------------------ *)
(* SQL execution (same pipeline as the REPL in main.ml)                *)
(* ------------------------------------------------------------------ *)

let run_sql sql =
  match Sql_parser.Parser.parse sql with
  | exception Sql_parser.Lexer.Lexer_error msg -> Error ("Lexer error: " ^ msg)
  | exception Sql_parser.Parser.Parse_error msg -> Error ("Parse error: " ^ msg)
  | stmt ->
    let type_error =
      try
        let schema = Sql_parser.Storage.get_schema db in
        Sql_parser.Types.check_statement schema stmt;
        None
      with
      | Sql_parser.Types.Type_error msg -> Some ("Type error: " ^ msg)
      | Sql_parser.Storage.Storage_error _ ->
        (* Table might not exist yet for CREATE TABLE *)
        None
    in
    (match type_error with
     | Some msg -> Error msg
     | None ->
       (try
          let plan = Sql_parser.Planner.plan_statement stmt in
          Ok (Sql_parser.Executor.execute db plan)
        with
        | Sql_parser.Executor.Exec_error msg -> Error ("Execution error: " ^ msg)
        | Sql_parser.Storage.Storage_error msg -> Error ("Storage error: " ^ msg)))

(* ------------------------------------------------------------------ *)
(* HTTP plumbing                                                       *)
(* ------------------------------------------------------------------ *)

let status_text = function
  | 200 -> "OK"
  | 400 -> "Bad Request"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | _ -> "Internal Server Error"

let send_response fd status body =
  let resp =
    Printf.sprintf
      "HTTP/1.1 %d %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status (status_text status) (String.length body) body
  in
  let len = String.length resp in
  let rec write_all off remaining =
    if remaining > 0 then begin
      let n = Unix.write_substring fd resp off remaining in
      write_all (off + n) (remaining - n)
    end
  in
  write_all 0 len

let route meth path body =
  match meth, path with
  | "GET", "/health" -> (200, {|{"status":"ok","service":"parseql"}|})
  | _, "/health" -> (405, json_error "method not allowed; use GET")
  | "POST", "/query" ->
    let sql = String.trim body in
    if sql = "" then (400, json_error "empty request body; expected a SQL statement")
    else
      (match run_sql sql with
       | Ok result -> (200, json_of_result result)
       | Error msg -> (400, json_error msg))
  | _, "/query" -> (405, json_error "method not allowed; use POST")
  | _ -> (404, json_error "not found")

let handle_connection fd =
  let ic = Unix.in_channel_of_descr fd in
  let read_line () =
    let line = input_line ic in
    let n = String.length line in
    if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line
  in
  let request_line = read_line () in
  match String.split_on_char ' ' request_line with
  | meth :: target :: _ ->
    (* Headers: we only care about Content-Length. *)
    let content_length = ref 0 in
    let rec read_headers () =
      let line = read_line () in
      if line <> "" then begin
        (match String.index_opt line ':' with
         | Some i ->
           let key = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
           let value =
             String.trim (String.sub line (i + 1) (String.length line - i - 1))
           in
           if key = "content-length" then
             (try content_length := int_of_string value with _ -> ())
         | None -> ());
        read_headers ()
      end
    in
    read_headers ();
    let body =
      if !content_length > 0 then begin
        let buf = Bytes.create !content_length in
        really_input ic buf 0 !content_length;
        Bytes.to_string buf
      end else ""
    in
    let path =
      match String.index_opt target '?' with
      | Some i -> String.sub target 0 i
      | None -> target
    in
    let status, response_body = route meth path body in
    send_response fd status response_body;
    Printf.printf "%s %s -> %d\n%!" meth path status
  | _ -> send_response fd 400 (json_error "malformed request line")

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen sock 16;
  Printf.printf "sql-parser HTTP server listening on port %d\n%!" port;
  while true do
    let fd, _addr = Unix.accept sock in
    (try handle_connection fd
     with
     | End_of_file -> ()
     | exn ->
       (try send_response fd 500 (json_error (Printexc.to_string exn))
        with _ -> ()));
    (try Unix.close fd with Unix.Unix_error _ -> ())
  done
