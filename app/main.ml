open Core
open Sexplib
open Sexp
open Mylib
open Parse
open Interpret
open Verify
open Solve

let () =
  let p = "(if (>= x 0)
            skip
            (set! x (- 0 x)))" in
  let prog = Parse.read_prog (Sexp.of_string p) in
  let env = Interpret.Environment.empty |> Interpret.Environment.set ~key:"x" ~data:(-3) in
  let res = Interpret.interpret env prog in
  print_endline (res |> [%sexp_of: int Interpret.Environment.t] |> Sexp.to_string);
  let precond = "(= x c)" |> Sexp.of_string |> Parse.read_bool in
  let postcond = "(and (>= x 0)
             (or (= x c)
                 (= x (- 0 c))))" |> Sexp.of_string |> Parse.read_bool in
  Verify.calc_cond precond prog postcond |> Solve.solve_easy |> print_endline;
  let p2 = "(if (>= x 0)
            skip
            skip)" in
  let prog2 = Parse.read_prog (Sexp.of_string p2) in
  Verify.calc_cond precond prog2 postcond |> Solve.solve_easy |> print_endline;
  

