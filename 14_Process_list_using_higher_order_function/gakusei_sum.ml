(*
gakusei_sum:
  outline: receive gakusei_t list and return sum of its tokuten
  type: gakusei_t list -> int
  subfunc: gakusei_add
gakusei_add:
  outline: receive int and gakusei_t and return gakusei_t's tokuten + int value
  type: gakusei_t -> int -> int
*)

let gakusei_add gakusei before = match gakusei with {tensuu = t}
  -> t + before

let test1 = gakusei_add gakusei1 5 = 93

let gakusei_sum lst = List.fold_right gakusei_add lst 0

let test2 = gakusei_sum list1 = 11 * 26