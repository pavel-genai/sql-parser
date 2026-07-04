# sql-parser

[![CI](https://github.com/ai-pavel/sql-parser/actions/workflows/ci.yml/badge.svg)](https://github.com/ai-pavel/sql-parser/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/ai-pavel/sql-parser/branch/main/graph/badge.svg)](https://codecov.io/gh/ai-pavel/sql-parser)

An OCaml SQL parser and query executor with CSV-backed storage.

## Features

- Lexer and recursive-descent parser for a SQL subset
- Supported statements: SELECT (WHERE, ORDER BY, LIMIT, JOIN), INSERT, CREATE TABLE, DROP TABLE
- Typed AST with a type checker for column references and types
- CSV-backed storage engine (each table is a .csv file)
- Query planner that converts AST to physical plans (scan, filter, project, sort, join)
- Query executor
- Interactive REPL

## Building

```
opam install . --deps-only
dune build
```

## Running

```
dune exec sql_parser
```

## Testing

```
dune runtest
```

## Project Structure

```
lib/
  ast.ml        - Typed AST definitions
  lexer.ml      - SQL tokenizer
  parser.ml     - Recursive descent parser
  types.ml      - Type checker for column references and types
  storage.ml    - CSV-backed storage engine
  planner.ml    - Query planner (AST -> physical plan)
  executor.ml   - Physical plan executor
bin/
  main.ml       - REPL entry point
test/
  test_sql_parser.ml - Alcotest test suite
```
