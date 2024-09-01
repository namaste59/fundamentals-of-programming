(*
saita_ketsueki
type_def:
  input: person_t list:
        - []
        - first :: rest 
  output: string
purpose:
  outline:receive person_t list and return the ketsueki that has biggest number of the four types
  header: ketsueki_shukei: person_t list -> string

*)

(*
max_set_of_four
type_def:
  input: int * int * int * int
  output: int
purpose:
  outline: receive a set that has four values and return the address in which value has the most biggest number
  limitaion: if there are two values that have the beggest number, return earlier one
*)

let max_set_of_four set = match set with
  (a, b, c, d) ->
    if a = 0 && b = 0 && c = 0 && d == 0 then -1
    else if a >= b && a >= b && a >= b then 1
    else if b >= c && b >= d then 2
    else if c >= d then 3
    else 4

let rec saita_ketsueki lst = 
  let set = ketsueki_shukei lst in
      let v = max_set_of_four set in 
      if v = -1 then "N"
      else if v = 1 then "A"
      else if v = 2 then "B"
      else if v = 3 then "AB"
      else "O"
    

let test1 = saita_ketsueki person_list1 = "A"
let test2 = saita_ketsueki person_list2 = "A"
let test3 = saita_ketsueki person_list3 = "N"
let test3 = saita_ketsueki person_list4 = "B"