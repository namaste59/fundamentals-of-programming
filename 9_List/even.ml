(*
data_def:
  input: int list
    - []
    - first :: rest //rest is self reference type
  output: int list, it is the same type as that of input
purpose:
  outline: return list that has the even items out of received list
  function: even : int list -> int list
*)

let rec even lst = match lst with
    [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest
                                        else even rest

(*test*) 

let test1 = even [] = []
let test2 = even [0; 1; 2; 3] = [0; 2]