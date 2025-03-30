(*Question 1*)
type 'a pointer = 'a option ref;;

exception Segfault;;

let malloc x : 'a pointer = ref (Some x);;

let null () : 'a pointer = ref None;;

let deref x =
  match !x with 
  | None -> raise Segfault
  | Some r -> r;;

let assign p v = p := Some v;;
type 'a nodeM = { value : 'a; next : 'a nodeM pointer };; 
type 'a stack = {top : 'a nodeM pointer};;

exception EmptyStack;;

let push (s:'a stack) v =
  match !(s.top) with
  | None -> s.top := Some {value = v; next = ref None} 
  | Some n -> 
    let new_node = {value = v; next = malloc n} in (* create a new node*)
    s.top :=  Some new_node;; (* update the top pointer*)

let pop (s:'a stack) =
  match !(s.top) with
  | None -> raise EmptyStack
  | Some n ->let v = n.value in
     s.top := !(n.next);
    v;;


(*Question 2*)
type 'a ary_element = { mutable value : 'a;};; 
type 'a array = 'a ary_element list;;

exception Negative_Index;;
exception Exceed_Length;;

let ( |~ ) (a:'a array) i : 'a array = a @ [ {  value = i } ];;

let rec helpfun (a:'a array) i cnt=
    if i < 0 then raise Negative_Index
    else if cnt < i then 
      match a with
      | _::t -> helpfun t i (cnt + 1)
      | [] -> raise Exceed_Length
    else if cnt = i then
      match a with
      | h::_ -> h
      | [] -> raise Exceed_Length
    else raise Exceed_Length;;

let ( |. ) (a:'a array) i =
  let ele = helpfun a i 0 in
  ele.value;;

let ( <~ ) ((a: 'a array), (n: int)) (v: 'a) : unit =
  let ele = helpfun a n 0 in
  ele.value <- v;;
