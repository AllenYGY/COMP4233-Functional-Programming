open OUnit2
open Lab_3_Xor

let tests = "test suite for Xor" >::: [
  "A is True, B is True, A Xor B is False" >:: ( fun _ -> assert_equal false (  true &+ true)); 
  "A is True, B is False, A Xor B is True" >:: ( fun _ -> assert_equal true (  true &+ false)); 
  "A is False, B is True, A Xor B is True" >:: (fun _ -> assert_equal true (  false &+ true));
  "A is False, B is False, A Xor B is False" >:: (fun _ -> assert_equal false ( false  &+ false));
]

let _ = run_test_tt_main tests