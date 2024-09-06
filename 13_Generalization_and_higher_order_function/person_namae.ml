type person_t = {
  name : string;
  shincho: int;
  tanjobi: int*int; 
  ketsueki: string;
}

(*
purpose:
  outline: receive person_t list and return its name list
  header: person_namae: person_t list -> string list
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

let f_person_namae person = match person with {name = n; shincho = s; tanjobi = t; ketsueki = k}
-> n 

let person_namae lst = List.map f_person_namae lst
(*mapは、空のリストを渡したとき、空のリストを返す*)


let test1 = person_namae person_list1 = ["Adam"; "Barbara"; "Cho"; "Dilan"; "Elizabeth";]
let test2 = person_namae person_list3 = []