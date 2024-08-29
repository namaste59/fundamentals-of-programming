(*
  purpose:
    args: The number of clanes and turtles
    retvals: The number of total legs
    type: int -> int -> int
  test cases:
    1:
      args: clane = 10, turtles = 5
      retvals: 40
    2:
      args: clane = 0, turtles = 0
      retvals: 0
    3
      args: clane = 8, turtles = 1
      retvals: 20
*)

let tsurukame_no_ashi x y = 2 * x + 4 * y

let test1 = (tsurukame_no_ashi 10 5 = 40)
let test2 = (tsurukame_no_ashi 0 0 = 0)
let test3 = (tsurukame_no_ashi 8 1 = 20)