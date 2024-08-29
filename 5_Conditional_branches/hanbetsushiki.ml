(*
purpose: calculate Discriminant of Quadratic equation
  args: a, b, c (a * x^2 + b * x + c = 0, a != 0)
  retvals: discriminant
  type: float -> float -> float -> float
test:
  case 1:
    args: 1.0 2.0 3.0
    retvals: -8.0
  case 2:
    args: 0.5 4.0 2.0
    retvals: 12.0
  case 3:
    args: 1.5 0.0 0.0
    retvals: 0.0
  case 4:
    args: -1.0 -2.0 2.0
    retvals: 16.0
*)

let hanbetsushiki a b c = b ** 2.0 -. 4.0 *. a *. c

let test1 = (hanbetsushiki 1.0 2.0 3.0 = -8.0)
let test2 = (hanbetsushiki 0.5 4.0 2.0 = 12.0)
let test3 = (hanbetsushiki 1.5 0.0 0.0 = 0.0)
let test4 = (hanbetsushiki (-1.0) (-2.0) 2.0 = 12.0)