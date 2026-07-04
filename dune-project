(lang dune 3.0)
(name sql_parser)
(version 0.1.0)
(generate_opam_files true)

(source (github pavel-genai/sql-parser))
(license MIT)
(authors "Pavel")
(maintainers "Pavel")

(package
 (name sql_parser)
 (synopsis "An OCaml SQL parser and query executor")
 (description "A SQL subset parser with CSV-backed storage engine, query planner, and executor")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (alcotest (and :with-test (>= 1.6.0)))
  (bisect_ppx (and :with-test (>= 2.6)))))
