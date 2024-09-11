(*
fold_left:
  outline: Receive function f, initial value init and list and return the result of inputting an item to f from the first item of the list. 
  type: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
*)

let rec fold_left f init lst = 
  let rec folding f init lst result = match lst with
    [] -> result
  | first :: rest -> folding f init rest (f result first) in
  folding f init lst init

let test1 = fold_left (fun x y -> x + y) 0 [1; 2; 3; 4] = 10
let test2 = fold_left (fun x y -> x ^ y) "" ["す"; "き"; "や"; "き"] = "すきやき"