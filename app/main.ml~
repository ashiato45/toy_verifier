open Core
open Sexplib
open Sexp
open Parse

let () =
  let p = "(if (>= x 0)
            skip
            (set! x (- 0 x)))" in
  let prog = Parse.read_prog (Sexp.of_string p) in
  print_endline (prog |> [%sexp_of: Parse.prog] |> Sexp.to_string)

