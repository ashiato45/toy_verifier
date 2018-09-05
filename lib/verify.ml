open Core
open Sexplib
open Sexp
open Testprog
open Parse

let subst_pbool (cond:pbool) (var:string) (replacing:pnum) : pbool =
  assert false

let calc_weakest_precond (prog:prog) (post:pbool) : pbool*(pbool list) =
  let rec help (prog:prog) (post:pbool) (acc:pbool list) : pbool*(pbool list) =
    match prog with
    | PSkip -> (post, acc)
    | PSet (var, n) -> (subst_pbool post var n, acc)
    | PIf (cond, t, f) -> (
      let (pos_cond, acc) = help t post acc in
      let (neg_cond, acc) = help f post acc in
      (BOr (BAnd (cond, pos_cond), BAnd (BNot cond, neg_cond)), acc)
    )
    | PWhile (cond, inv, p) -> (
      let (inv_pre_p, acc) = help p inv acc in
      let acc = (BImplies (BAnd (BNot cond, inv), post))::(BImplies (BAnd (cond, inv), inv_pre_p))::acc in
      (inv, acc)
    )
    | PBlock xs -> (List.fold_right ~f:(fun prog (post, acc) -> help prog post acc) ~init:(post, acc) xs) in
  help prog post []


