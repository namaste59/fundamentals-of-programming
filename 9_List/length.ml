(*
input:
  type: int list:
    - []
    - first :: rest //rest is self-reference type
output:
  type: int
purpose: 
  outline: count the number of a received list items
  func: length: int list -> int
case:
  case1:
    input: []
    output: 0
  case2:
    input: [1; 3; 5]
    output: 3
*)

let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

let test1 = length [] = 0
let test2 = length [1; 3; 5] = 3