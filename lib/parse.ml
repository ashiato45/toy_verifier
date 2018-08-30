open Core
open Sexplib
open Sexp
open Testprog

type pnum = NNum of int
         | NVar of string
         | NPlus of pnum * pnum
         | NMinus of pnum * pnum [@@deriving compare, sexp]

type pbool = BEqual of pnum * pnum
           | BGt of pnum * pnum
           | BLt of pnum * pnum
           | BGe of pnum * pnum
           | BLe of pnum * pnum
           | BAnd of pbool * pbool
           | BOr of pbool * pbool
           | BNot of pbool [@@deriving compare, sexp]

type prog = PSkip
          | PSet of string * pnum
          | PIf of pbool * prog * prog
          | PWhile of pbool * pbool * prog
          | PBlock of prog list [@@deriving compare, sexp]

exception Parse_error

let rec read_num (s:Sexp.t): pnum = 
  (match s with
   | List [Atom "+"; a; b] -> NPlus (read_num a, read_num b)
   | List [Atom "-"; a; b] -> NMinus (read_num a, read_num b)
   | Atom s -> (
     match (int_of_string_opt s) with
     | None -> NVar s
     | Some n -> NNum n
   )
   | otherwise -> raise Parse_error
  )

let rec read_bool (s:Sexp.t): pbool = 
  (match s with
   | List [Atom "="; a; b] -> BEqual (read_num a, read_num b)
   | List [Atom ">"; a; b] -> BGt (read_num a, read_num b)
   | List [Atom "<"; a; b] -> BLt (read_num a, read_num b)
   | List [Atom ">="; a; b] -> BGe (read_num a, read_num b)
   | List [Atom "<="; a; b] -> BLe (read_num a, read_num b)
   | List [Atom "and"; a; b] -> BAnd (read_bool a, read_bool b)
   | List [Atom "or"; a; b] -> BOr (read_bool a, read_bool b)
   | List [Atom "not"; a] -> BNot (read_bool a) 
   | otherwise -> raise Parse_error
  )

let rec read_prog (s:Sexp.t) : prog = 
  (match s with 
   | Atom "skip" -> PSkip
   | List [Atom "set!"; Atom v; n] -> PSet (v, read_num n)
   | List [Atom "if"; cond; a; b] -> PIf (read_bool cond, read_prog a, read_prog b)
   | List [Atom "while"; cond; invariant; p] -> PWhile (read_bool cond, read_bool invariant, read_prog p)
   | List ((Atom "block")::xs) -> PBlock (xs |> List.map ~f:read_prog)
   | otherwise -> raise Parse_error
  )
let%expect_test "read_prog1" =
  let prog = read_prog (Sexp.of_string Testprog.p) in
  print_endline (prog |> [%sexp_of: prog] |> Sexp.to_string);
  [%expect {| (PIf(BGe(NVar x)(NNum 0))PSkip(PSet x(NMinus(NNum 0)(NVar x)))) |}]
