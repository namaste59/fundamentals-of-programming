(*
data_def:
  gakusei_t: represent name,tensuu, and seiseki of gakusei
purpose:
  outline: receive gakusei_t list and return sorted list of items of received list
  header: gakusei_sort: gakusei_t list -> gakusei_t list
*)
type gakusei_t = {
  namae : string;
  tensuu : int;
  seiseki : string;
}

let gakusei1 = {namae = "gakusei tarou"; tensuu = 88; seiseki = "A"}
let gakusei2 = {namae = "gakusei jiro"; tensuu = 77; seiseki = "A"}
let gakusei3 = {namae = "gakusei saburo"; tensuu = 66; seiseki = "B"}
let gakusei4 = {namae = "gakusei shiro"; tensuu = 55; seiseki = "B"}

let list1 = [gakusei1; gakusei2; gakusei3; gakusei4]
let list2 = [gakusei4; gakusei2; gakusei1]
let list3 = []

(*
count_A:
  purpose: receive gakusei_t list and return the number of its seiseki = "A"
  type: gakusei_t list -> int
  subfunc:  is_A
is_A:
  purpose receive gakusei_t and return bool whether received value's seiseki is "A"
  type: gakusei_t -> bool
*)

let is_A gakusei = match gakusei with {namae = n; tensuu = t; seiseki = s}
  -> s = "A"

let test1 = is_A gakusei1 = true
let test2 = is_A gakusei3 = false

let count_A lst = List.length(List.filter is_A lst)

let test3 = count_A list1 = 2
let test4 = count_A list3 = 0