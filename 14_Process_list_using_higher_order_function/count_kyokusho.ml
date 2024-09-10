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

let count_kyokusho lst seiseki0 = 
  let count gakusei = match gakusei with {seiseki = s}
    -> s = seiseki0 in
      List.length(List.filter count lst)

let test1 = count_kyokusho list1 "A" = 2
let test2 = count_kyokusho list2 "B" = 1
let test3 = count_kyokusho list3 "A" = 0
let test4 = count_kyokusho list1 "Z" = 0