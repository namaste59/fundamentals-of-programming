(*
quicksort:
  outline: receive int list and return int list that sort the received list by ascending order
  type: int list -> int list
  sub_function: take_less, take_greater, select_pivot
select_pivot:
  outline: receive int list and return int value that is not the smallest value of the received list
take_less:
  outline: receive int list and int and return int list that has values which are less than received int in received list
  type: int list -> int -> int list
take_greater:
  outline: the greater version of take_less
  type: int list -> int -> int
*)

let list1 = [9; 8; 7; 6; 5]
let list2 = [4; 6; 1; 7]
let list3 = [1; 1; 1; 1; 1; 1; 1]
let list4 = []

(*let select_pivot lst *)

let take_less lst pivot = List.filter (fun n -> n < pivot) lst

let take_greater lst pivot = List.filter(fun n -> n >= pivot) lst

let rec quicksort lst = match lst with
    [] -> []
  | first :: rest -> quicksort (take_less rest first)
                     @ [first]
                     @ quicksort (take_greater rest first)


(*test*)

let test1 = quicksort list1 = [5; 6; 7; 8; 9]
let test2 = quicksort list2 = [1; 4; 6; 7]
let test3 = quicksort list3 = [1; 1; 1; 1; 1; 1; 1]
let test4 = quicksort list4 = []