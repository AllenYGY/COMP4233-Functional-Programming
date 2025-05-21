open OUnit2
open Interp
open Ast
open Main

let parse_i name i s =
  name >:: (fun _ -> assert_equal (i) (parse s))

let parse_s name i s =
  name >:: (fun _ -> assert_equal (i) (string_of_val (parse s)))

let evalu_i name i s =
  name >:: (fun _ -> assert_equal (string_of_int i) (interp s))

let tests = [
  parse_i "Parse1 int 22" (Int 22) "22";
  parse_i "Parse2 int -1" (Int (-1)) "-1";
  parse_i "Parse3 plus 1+1" (Binop (Add, Int 1, Int 1)) "1+1";
  parse_i "Parse4 plus 2*3" (Binop (Mult, Int 2, Int 3)) "2*3";
  parse_i ("Parse5 x") (Id "x") "x";
  parse_i ("Parse6 let x = 1 in x") (Let ("x", (Int 1), (Id "x"))) "let x = 1 in x";
  evalu_i "Evalu1 int -1" (-1) "-1";
  evalu_i "Evalu2 int 1+1" (2) "(1)+1";
  evalu_i "Evalu3 int 2*3" (6) "2*3";
  evalu_i "Evalu4 int 2*3+5" (11) "2*3+5";
  evalu_i "Evalu5 let x = 1 in x" (1) "let x = 1 in x";
]

let _ = run_test_tt_main ("suite" >::: tests)