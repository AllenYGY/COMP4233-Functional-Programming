(*Page 3 Reference & Dereference*)
let a = 1;;
let b = 1;;
let c = ref 1;;
let d = ref a;;
let e = ref b;;

!c = !d;; (*True, because both !c and !d are dereferenced to 1.*)
c == d;; (*False, because c and d are two variables with different memery locations.*)
c = d;; (*True, because 'a' and '1' are alias.*)
d = e;; (*Same as above*)
d == e;; (*False, because d and e are at different locations.*)

(*Page 4 List Construction*)
exception Negative_Index;;
let rec check_lst_add i l1 l2 =
  if i = 0 then l1 == l2
  else if i < 0 then raise Negative_Index
  else
    match l1 with
    | [] -> false 
    | _ :: t -> check_lst_add (i - 1) t l2;;

let l1 = [1;2;3];;
let l2 = 0 :: l1;;
let l3 = [0] @ l1;;
let l4 = l1 @ l1 @ l1;;

(*Page 6 Loop Fibonacci*)
let fib n = 
  let i = ref 0 in
  let fa = ref 1 in 
  let fb = ref 1 in  
  while !i < n do 
    let t = !fa + !fb in 
    fa := !fb; 
    fb := t;
    i := !i + 1
  done;
  !fa;;

(*Page 7 Pointer*)
type 'a pointer = 'a option ref;;
exception Segfault;;
let malloc x : 'a pointer = ref (Some x);;
let null x : 'a pointer = ref None;;
let deref x = 
  match !x with 
  | None -> raise Segfault
  | Some v -> v;;
let assign p v = p := Some v;;

(*Page 8 New Stack*)
type 'a nodeM = {
  value : 'a option;
  next : 'a nodeM pointer
};;
type 'a stack = {top : 'a nodeM pointer};;
exception EmptyStack;;
let push (s:'a stack) v =
  match !(s.top) with 
  | None -> s.top := Some {value = Some v; next = ref None}
  | Some n -> 
    let new_node = Some {value = Some v; next = malloc n} in 
    s.top := new_node;;
