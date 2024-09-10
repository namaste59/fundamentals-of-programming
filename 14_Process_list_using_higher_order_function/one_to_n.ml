(*
one_to_n:
  outline: receive n and return sum of 1 to n
  type: int -> int
  restrinction: using enumerate
enumerate:
  outline: receive n and return list that values are 1 to n
*)

let rec enumerate n =
  if n = 0 then []
  else n :: enumerate (n - 1)

let one_to_n n = List.fold_right (fun first second -> first + second) (enumerate n) 0 

let test1 = one_to_n 10 = 55
let test2 = one_to_n 0 = 0
  
let fac n = List.fold_right (fun first second -> first * second) (enumerate n) 1

let test3 = fac 5 = 120
let test4 = fac 0 = 1