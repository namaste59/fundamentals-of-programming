(*
purpose: counting the number of roots of a quadratic equation
  args: a b c (a != 0)
  retval: count
  type: float -> float -> float -> int
test:
  case1(D > 0):
    args: 1.0 5.0 1.0
    retval: 2
  case2(D = 0):
    args: 1.0 2.0 1.0
    retval: 1
  case3(D < 0):
    args: 1.0 1.0 1.0
    retval: 0
  case4(a, b, c are negative numbers)
    args: (-3.0) (-2.0) (-1.0)
    retval: 0
*)
let hanbetsushiki a b c = b ** 2.0 -. 4. *. a *. c

let kai_no_kosuu a b c = 
  if hanbetsushiki a b c > 0.0 then 2
  else if hanbetsushiki a b c = 0.0 then 1
  else 0 

let test1 = (kai_no_kosuu 1.0 5.0 1.0 = 2)
let test2 = (kai_no_kosuu 1.0 2.0 1.0 = 1)
let test3 = (kai_no_kosuu 1.0 1.0 1.0 = 0)
let test4 = (kai_no_kosuu (-3.0) (-2.0) (-1.0) = 0)