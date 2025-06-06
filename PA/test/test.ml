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

let evalu_b name i s =
  name >:: (fun _ -> assert_equal (string_of_bool i) (interp s))

let evalu_list name expected s =
  name >:: (fun _ -> assert_equal expected (interp s))

let evalu_s name expected s =
  name >:: (fun _ -> assert_equal expected (interp s))

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
  evalu_b "Evalu6 let x = (fun a -> (if (a<=1) then true else false)) in (x 1)" (true) "let x = (fun a -> (if (a<=1) then true else false)) in (x 1)";
  
  evalu_list "List test" "true :: []" "let x = (fun a -> (if (a<=1) then true else false)) in ((x 1)::[])";

  parse_i "Parse7 bool true" (Bool true) "true";
  parse_i "Parse8 bool false" (Bool false) "false";
  parse_i "Parse9 empty list" (List) "[]";
  parse_i "Parse10 cons" (Binop (Cons, Int 1, List)) "1::[]";
  parse_i "Parse11 leq" (Binop (Leq, Int 1, Int 2)) "1<=2";
  parse_i "Parse12 and" (Binop (And, Bool true, Bool false)) "true^false";
  parse_i "Parse13 fun" (Fun ("x", Id "x")) "fun x -> x";
  parse_i "Parse14 app" (App (Id "f", Int 1)) "f 1";
  parse_i "Parse15 if" (If (Bool true, Int 1, Int 2)) "if true then 1 else 2";
  parse_i "Parse16 nested let" (Let ("x", Int 1, Let ("y", Int 2, Binop (Add, Id "x", Id "y")))) "let x = 1 in let y = 2 in x + y";

  evalu_i "Arith1 zero" 0 "0";
  evalu_i "Arith2 addition" 7 "3+4";
  evalu_i "Arith3 multiplication" 15 "3*5";
  evalu_i "Arith4 complex" 14 "2+3*4";
  evalu_i "Arith5 parentheses" 20 "(2+3)*4";
  evalu_i "Arith6 negative" (-5) "-5";
  evalu_i "Arith7 negative operation" (-2) "-5+3";

  evalu_b "Bool1 true" true "true";
  evalu_b "Bool2 false" false "false";
  evalu_b "Bool3 and true" true "true^true";
  evalu_b "Bool4 and false" false "true^false";
  evalu_b "Bool5 and false false" false "false^false";

  evalu_b "Comp1 leq true" true "3<=5";
  evalu_b "Comp2 leq false" false "5<=3";
  evalu_b "Comp3 leq equal" true "3<=3";
  evalu_b "Comp4 complex comp" true "(2+3)<=(4*2)";

  evalu_i "Let1 simple" 5 "let x = 5 in x";
  evalu_i "Let2 addition" 8 "let x = 3 in x + 5";
  evalu_i "Let3 nested" 10 "let x = 3 in let y = 7 in x + y";
  evalu_i "Let4 shadowing" 7 "let x = 3 in let x = 7 in x";
  evalu_i "Let5 complex" 15 "let x = 2 in let y = x + 3 in x * y + 5";

  evalu_s "Fun1 identity" "fun x -> ..." "fun x -> x";
  evalu_i "Fun2 app simple" 5 "(fun x -> x) 5";
  evalu_i "Fun3 app add" 8 "(fun x -> x + 3) 5";
  evalu_i "Fun4 nested fun" 10 "(fun x -> (fun y -> x + y) 3) 7";
  evalu_b "Fun5 bool fun" false "(fun x -> x^true) false";

  evalu_i "If1 true branch" 1 "if true then 1 else 2";
  evalu_i "If2 false branch" 2 "if false then 1 else 2";
  evalu_i "If3 complex guard" 10 "if 3<=5 then 10 else 20";
  evalu_i "If4 nested if" 1 "if true then (if false then 2 else 1) else 3";

  evalu_s "List1 empty" "[]" "[]";
  evalu_s "List2 single int" "5 :: []" "5::[]";
  evalu_s "List3 single bool" "true :: []" "true::[]";
  evalu_s "List4 multiple ints" "1 :: 2 :: 3 :: []" "1::2::3::[]";
  evalu_s "List5 multiple bools" "true :: false :: []" "true::false::[]";
  evalu_s "List6 mixed with let" "5 :: []" "let x = 5 in x::[]";

  evalu_i "Complex1 conditional function" 1 "let check = (fun n -> if n<=1 then 1 else n+10) in check 0";
  evalu_i "Complex2 let with fun" 25 "let square = (fun x -> x*x) in square 5";
  evalu_i "Edge1 zero mult" 0 "0*100";
  evalu_i "Edge2 zero add" 5 "0+5";
  evalu_b "Edge3 equal leq" true "5<=5";

]

let _ = run_test_tt_main ("suite" >::: tests)
