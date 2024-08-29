(*
Purpose: Judge whether a quadric equation has imaginary number
  args: a b c(a != 0)
  retvals: true or false
  type: int -> int -> int -> bool
*)

let hanbetsushiki a b c = b ** 2.0 -. 4.0 *. a *. c

let kyosuukai a b c = 
  if hanbetsushiki a b c < 0.0 then true
  else false  

let test1 = kyosuukai 1.0 2.0 1.0 = false (* D = 0.0 *)
let test2 = kyosuukai 3.0 (-1.0) 4.0 = true (* D = -11.0 *)
let test3 = kyosuukai (-1.0) 5.0 3.0 = false(* D = 37.0 *)