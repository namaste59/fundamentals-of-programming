(*
concat_fold_right: 
  outline: receive string list and receive string that concate items in the received list from the first item
  type: string list -> string
*)

let concat_fold_right lst = List.fold_right (^) lst ""

let test1 = concat_fold_right ["あ"; "い"; "し"; "て"; "る"] = "あいしてる"
let test2 = concat_fold_right ["さみだ"; "れ"] = "さみだれ"
let test3 = concat_fold_right [] = ""