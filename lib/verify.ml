open Core
open Sexplib
open Sexp
open Testprog
open Parse

let rec subst_pnum (var:string) (replacing:pnum) (n:pnum) : pnum = 
  match n with
  | NVar v -> (
      if [%compare: string] v var = 0 then replacing else n
    )
  | NPlus (l, r) -> NPlus (subst_pnum var replacing l, subst_pnum var replacing r)
  | NMinus (l, r) -> NMinus (subst_pnum var replacing l, subst_pnum var replacing r)
  | otherwise -> n
                          

let rec subst_pbool (var:string) (replacing:pnum) (cond:pbool) : pbool =
  let subst_pbool' = subst_pbool var replacing in
  let subst_pnum' = subst_pnum var replacing in
  match cond with
  | BEqual (l, r) -> BEqual (subst_pnum' l, subst_pnum' r)
  | BGt (l, r) -> BGt (subst_pnum' l, subst_pnum' r)
  | BLt (l, r) -> BLt (subst_pnum' l, subst_pnum' r)
  | BGe (l, r) -> BGe (subst_pnum' l, subst_pnum' r)
  | BLe (l, r) -> BLe (subst_pnum' l, subst_pnum' r)
  | BAnd (l, r) -> BAnd (subst_pbool' l, subst_pbool' r)
  | BOr (l, r) -> BOr (subst_pbool' l, subst_pbool' r)
  | BNot a -> BNot (subst_pbool' a)
  | BImplies (l, r) -> BImplies (subst_pbool' l, subst_pbool' r)

let calc_weakest_precond (prog:prog) (post:pbool) : pbool*(pbool list) =
  let rec help (prog:prog) (post:pbool) (acc:pbool list) : pbool*(pbool list) =
    match prog with
    | PSkip -> (post, acc)
    | PSet (var, n) -> (subst_pbool var n post, acc)
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


let calc_cond (precond:pbool) (p:prog) (postcond:pbool) : pbool list =
  let (weakest, invcheck) = calc_weakest_precond p postcond in
  (BImplies (precond, weakest))::invcheck
