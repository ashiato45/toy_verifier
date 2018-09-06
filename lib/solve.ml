open Core
open Sexplib
open Sexp
open Testprog
open Parse
open Verify
open Z3
open Map

module VariableDict = Map.Make(String)

let get_variable (ctx:context) (vd: Expr.expr VariableDict.t ref) (v: string) : Expr.expr =
  match (VariableDict.mem !vd v) with
  | false -> (
    let newvar = (Expr.mk_const ctx (Symbol.mk_string ctx v) (Arithmetic.Integer.mk_sort ctx)) in
    vd := (VariableDict.set ~key:v ~data:newvar !vd);
    newvar
  )
  | true -> (VariableDict.find_exn !vd v)



let rec z3constr_of_pnum (ctx:context) (vd: Expr.expr VariableDict.t ref) (n: pnum) : Expr.expr =
  let z3constr_of_pnum' = z3constr_of_pnum ctx vd in
  match n with
  | NNum num -> (Arithmetic.Integer.mk_numeral_i ctx num)
  | NVar v -> (get_variable ctx vd v)
  | NPlus (a, b) -> (Arithmetic.mk_add ctx [z3constr_of_pnum' a; z3constr_of_pnum' b])
  | NMinus (a, b) -> (Arithmetic.mk_sub ctx [z3constr_of_pnum' a; z3constr_of_pnum' b])


let rec z3constr_of_pbool (ctx:context) (vd: Expr.expr VariableDict.t ref) (cond: pbool) : Z3.Expr.expr =
  let z3constr_of_pnum' = z3constr_of_pnum ctx vd in
  let z3constr_of_pbool' = z3constr_of_pbool ctx vd in
  match cond with
  | BEqual (a, b) -> (Boolean.mk_eq ctx (z3constr_of_pnum' a) (z3constr_of_pnum' b))
  | BGt (a, b) -> (Arithmetic.mk_gt ctx (z3constr_of_pnum' a) (z3constr_of_pnum' b))
  | BLt (a, b) -> (Arithmetic.mk_lt ctx (z3constr_of_pnum' a) (z3constr_of_pnum' b))
  | BGe (a, b) -> (Arithmetic.mk_ge ctx (z3constr_of_pnum' a) (z3constr_of_pnum' b))
  | BLe (a, b) -> (Arithmetic.mk_le ctx (z3constr_of_pnum' a) (z3constr_of_pnum' b))
  | BAnd (a, b) -> (Boolean.mk_and ctx [z3constr_of_pbool' a; z3constr_of_pbool' b])
  | BOr (a, b) -> (Boolean.mk_or ctx [z3constr_of_pbool' a; z3constr_of_pbool' b])
  | BNot a -> (Boolean.mk_not ctx (z3constr_of_pbool' a))
  | BImplies (a, b) ->  (Boolean.mk_implies ctx (z3constr_of_pbool' a) (z3constr_of_pbool' b))

type verification_result = VRSafe
                         | VRUnsafe of int VariableDict.t
                         | VRUnknown

let solve (conds:pbool list) : verification_result =
  let cfg = [("model", "true"); ("proof", "false")] in
  let ctx = (mk_context cfg) in
  let vd = ref (VariableDict.empty) in
  let constr = Boolean.mk_not ctx ( Boolean.mk_and ctx (List.map ~f:(z3constr_of_pbool ctx vd) conds)) in
  let solver = Solver.mk_solver ctx None in
  Solver.add solver [constr];
  let res = Solver.check solver [] in
  match res with
  | SATISFIABLE -> (
    let m = Solver.get_model solver in 
    match m with
    | None -> VRUnsafe (VariableDict.empty)
    | Some m -> (
      let assgn = VariableDict.fold ~init:(VariableDict.empty) ~f:(fun ~key:key ~data:data dic ->
                      match Model.get_const_interp_e m e with
                      | None -> dic
                      | Some e -> (VariableDict.set ~key:key ~data:(Arithmetic.Integer.get_int e) dic)
                    ) !vd in
      VRUnsafe assgn
    )
  )
  | UNSATISFIABLE -> VRSafe
  | UNKNOWN -> VRUnknown
