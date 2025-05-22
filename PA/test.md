## Test case 1

```ocaml
let x = (fun a -> (if (a<=1) then true else false)) in ((x 1)::[])
```

## Test case 3

```ocaml
let x = (fun a -> (if (a<=1) then true else false)) in (x 1) 
```

## Test case 2

```ocaml
let x = 3 in (let x = 1 in x) + x
let x = 1 in x + 2 * 3  
```
