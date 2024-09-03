(*
purpose:
  outline: receive two lists and return bool about whether the two lists has the same length
  header: equal_length: 'a list -> 'a list
*)

let rec equal_length lst1 lst2 = match (lst1, lst2) with
    ([], []) -> true
  |  ([], first2 :: rest2) -> false
  |  (first1 :: rest1, []) -> false
  |  (first1 :: rest1, first2 :: rest2) -> equal_length rest1 rest2

(*test*)
let list1 = [1; 2; 3; 4]
let list2 = [5; 6; 7]
let list3 = []
let list4 = ["a"; "b"; "c"]
let list5 = ["d"; "e"]


let test1 = equal_length list1 list2 = false
let test2 = equal_length list3 list3 = true
let test3 = equal_length list2 list4 = true