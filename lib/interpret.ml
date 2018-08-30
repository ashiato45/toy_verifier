open Core
open Parse
open Map

module Environment = Map.Make(String)

exception Undefined_variable
exception Interpretation_error
exception Invariant_violation

let rec interpret_num (e:int Environment.t) (n:pnum) : int = 
  match n with
  | NNum n -> n
  | NVar v -> (
    match Environment.find e v with
    | None -> raise Undefined_variable
    | Some n -> n
  )
  | NPlus (a, b) -> (interpret_num e a) + (interpret_num e b)
  | NMinus (a, b) -> (interpret_num e a) - (interpret_num e b)

let rec interpret_bool (e:int Environment.t) (b:pbool) : bool = 
  match b with
  | BEqual (a, b) -> (interpret_num e a) = (interpret_num e b)
  | BGt (a, b) -> (interpret_num e a) > (interpret_num e b)
  | BLt (a, b) -> (interpret_num e a) < (interpret_num e b)
  | BGe (a, b) -> (interpret_num e a) >= (interpret_num e b)
  | BLe (a, b) -> (interpret_num e a) <= (interpret_num e b)
  | BAnd (a, b) -> (interpret_bool e a) && (interpret_bool e b)
  | BOr (a, b) -> (interpret_bool e a) || (interpret_bool e b)
  | BNot a -> not (interpret_bool e a)

let rec interpret (e:int Environment.t) (p:prog) : int Environment.t = 
  match p with
  | PSkip -> e
  | PSet (v, n) -> Environment.set ~key:v ~data:(interpret_num e n) e
  | PIf (cond, a, b) -> (
     match (interpret_bool e cond) with
     | true -> interpret e a
     | false -> interpret e b
  )
  | PWhile (cond, inv, p) -> (
    match (interpret_bool e cond) with
    | false -> e
    | true -> (
      let e' = interpret e p in
      if (interpret_bool e' inv) then raise Invariant_violation;
      e'
    )
  ) 
  | PBlock [] -> e
  | PBlock (x::xs) -> (
    let e' = interpret e p in
    interpret e' (PBlock xs)
  )
let%expect_test "interpret1" =
  let prog = read_prog (Sexp.of_string Testprog.p) in
  let env = Environment.empty |> Environment.set ~key:"x" ~data:(-3) in
  let res = interpret env prog in
  print_endline (res |> [%sexp_of: int Environment.t] |> Sexp.to_string);
  [%expect {| ((x 3)) |}]
