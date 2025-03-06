(* Question 1 *)
let rec sub a b=
  if b = 0 then a 
  else (sub (pred(a)) (pred b));;


(* Question 3 *)
let fib_fast n =
  let arr = Array.make (n + 1) 1 in
  for i = 2 to n do
    arr.(i) <- arr.(i-1) + arr.(i-2);
  done;
  arr.(n);;