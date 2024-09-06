(*
purpose:
  outline: receive int list and return int list which has even values in the received list and using filter
  header: even_filter: int list -> int list
sub-function: is_even
  outline: receive int and return bool as to whether the int value is even.
  header: is_even: int -> bool
*)

let is_even n = (n mod 2 = 0)

let test1 = is_even 2 = true
let test2 = is_even 1 = false

let even_filter lst = List.filter is_even lst

let test3 = even_filter [1; 2; 3; 4; 5; 6] = [2; 4; 6]
let test4 = even_filter [0; 11; 5] = [0]
let test5 = even_filter[1; 3; 5] = []