(*
purpose: 
    argument: The number of clanes
    return value: The number of legs of the clanes
    type: int -> int
test case:
    1:
        arg: 1
        retval: 2
    2:
        arg: 0
        retval: 0
    3:
        arg: 100
        retval: 200
        
*)

let tsuru_no_ashi x = 2 * x 

(*テスト*)

let test1 = (tsuru_no_ashi 1 = 2)
let test2 = (tsuru_no_ashi 0 = 0)
let test3 = (tsuru_no_ashi 100 = 200)