(*
data_def:
  input: gakusei_t list
  output: gakusei_t
purpose:
  outline: receive gakusei_t list and return gakusei_t record having the maximum tensuu
  function: gakusei_max: gakusei_t list -> gakusei_t
*)

type gakusei_t = {
  namae : string;
  tensuu : int;
  seiseki : string;
}

let rec gakusei_max lst = match lst with
    [] -> {namae = ""; tensuu = min_int ; seiseki = ""}
  | {namae = n; tensuu = t; seiseki = s} as first :: rest -> 
    let g_max = gakusei_max rest in
      match g_max with
        {namae = n_max; tensuu = t_max; seiseki = s_max} ->
          if t > t_max
            then first
            else g_max

(*testing*)
let test1 = gakusei_max [
  {namae = "手巣都"; tensuu = 0; seiseki = "A"}
] = {namae = "手巣都"; tensuu = 0; seiseki = "A"}
let test2 = gakusei_max [] = {namae = ""; tensuu = min_int; seiseki = ""}
let test3 = gakusei_max [
  {namae = "手巣都"; tensuu = 0; seiseki = "A"};
  {namae = "test"; tensuu = 98; seiseki = "B"}
] = {namae = "test"; tensuu = 98; seiseki = "B"}