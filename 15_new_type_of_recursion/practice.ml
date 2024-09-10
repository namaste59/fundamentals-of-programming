(*
gcd:
  outline: receive two values and return the gcd of them
  type: int -> int -> int
*)

let rec gcd m n = 
  if n = 0
    then m
    else gcd n (m mod n)

let test1 = gcd 121 11 = 11

(*
sieve:
    outline: receive n and return int list that has all prime number not more than receive value(<= n)
    type: int list -> int list
    sub_func: enumerate
enumerate:
    outline: receive m(int), n(int), m <= n and return int list that have m ~ n
*)

let rec enumerate m n = 
  if m >= n + 1
  then []
  else m :: enumerate (m + 1) n 

let rec sieve lst = match lst with 
    [] -> []
  | first :: rest -> first :: sieve(List.filter (fun x -> x mod first <> 0) rest )

let test2 = sieve (enumerate 2 7) = [2; 3; 5; 7]
let test3 = sieve [] = []

let prime n = 
  let number_list = enumerate 2 n in
   sieve number_list