type person_t = {
  name : string;
  shincho: int;
  tanjobi: int*int; 
  ketsueki: string;
}

(*
type_def:
  input: person_t list:
        - []
        - first :: rest 
  output: person_t
purpose:
  outline:receive person_t list and count the number of each ketsueki types
  header: ketsueki_shukei: person_t list -> int * int * int * int
*)

(*type and list cases*)

let person1 = {name = "Adam"; shincho = 185; tanjobi = (12, 12); ketsueki = "A"}
let person2 = {name = "Barbara"; shincho = 165; tanjobi = (3, 31); ketsueki = "B"}
let person3 = {name = "Cho"; shincho = 194; tanjobi = (8, 12); ketsueki = "AB"}
let person4 = {name = "Dilan"; shincho = 176; tanjobi = (3, 12); ketsueki = "O"}
let person5 = {name = "Elizabeth"; shincho = 179; tanjobi = (9, 3); ketsueki = "A"}
let person6 = {name = "Elizabeth"; shincho = 179; tanjobi = (9, 3); ketsueki = "B"}

let person_list1 = [person1; person2; person3; person4; person5]
let person_list2 = [person2; person1; person3;]
let person_list3 = []
let person_list4 = [person1; person2; person3; person6]

let rec ketsueki_shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {name = n; shincho = s; tanjobi = t; ketsueki = k} :: rest -> 
      let (a, b, ab, o) = ketsueki_shukei rest in 
        if k = "A" then (a + 1, b, ab, o)
        else if k = "B" then (a, b + 1, ab, o)
        else if k = "AB" then (a, b, ab + 1, o)
        else (a, b, ab, o + 1)

(*test
let test1 = ketsueki_shukei person_list1 = (2, 1, 1, 1)
let test2 = ketsueki_shukei person_list2 = (1, 1, 1, 0)
let test3 = ketsueki_shukei person_list3 = (0, 0, 0, 0)
*)