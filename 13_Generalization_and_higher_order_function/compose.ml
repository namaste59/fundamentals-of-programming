(*
purpose: 
  outline: receive two functions and return one function that is composed of the received functions
  header: ('a -> 'b) -> ('c -> 'a) -> 'c -> 'c
*)

let compose f g =
  let h x = f (g x) in
  h


let time2 x = 2 * x
let add3 x = x + 3

let test1 = (compose time2 add3) 4 = 14
let test2 = (compose add3 time2) 4 = 11 