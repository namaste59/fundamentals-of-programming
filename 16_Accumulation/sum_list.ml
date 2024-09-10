(*
sum_list:
  purpose: receive int list and return int list that each value is the sum of the value and before
  type: int list -> int list
*)

let sum_list lst = 
  (*supple: receive lst & sum and return lst of sums*)
  (*sum is the sum of the values from the first to the former value*)
  let rec supple lst sum = match lst with 
    [] -> []
  | first :: rest -> sum + first :: supple rest (sum + first) in
    supple lst 0

let test1 = sum_list [1; 2; 3; 4] = [1; 3; 6; 10]
let test2 = sum_list [] = []