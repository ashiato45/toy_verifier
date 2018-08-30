open Core
open Sexplib
open Sexp
open Mylib
open Parse
open Interpret

let () =
  let p = "(if (>= x 0)
            skip
            (set! x (- 0 x)))" in
  let prog = Parse.read_prog (Sexp.of_string p) in
  let env = Interpret.Environment.empty |> Interpret.Environment.set ~key:"x" ~data:(-3) in
  let res = Interpret.interpret env prog in
  print_endline (res |> [%sexp_of: int Interpret.Environment.t] |> Sexp.to_string)

