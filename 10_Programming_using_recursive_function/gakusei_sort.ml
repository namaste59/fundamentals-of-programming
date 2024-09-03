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

let rec gakusei_insert gakusei lst = match lst with
    [] -> [gakusei]
  | ({namae = n_f; tensuu = t_f; seiseki = s_f} as first) :: rest -> match gakusei with {namae = n; tensuu = t; seiseki = s}
      -> if(t < t_f) then gakusei :: first :: rest
         else first :: gakusei_insert gakusei rest 



let rec gakusei_sort lst = match lst with
    [] -> []
  | first :: rest -> gakusei_insert first (gakusei_sort rest)

let test1 = gakusei_sort list1 = [gakusei4; gakusei3; gakusei2; gakusei1]
let test2 = gakusei_sort list2 = [gakusei4; gakusei2; gakusei1]
let test3 = gakusei_sort list3 = []