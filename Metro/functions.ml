(*hyoji*)
(*
data_def:
  input: ekimei_t
  output: string
purpose:
  outline: receive ekimei_t and return "{shozoku}, {kanji}(kana)"
  function: hyoji: ekimei_t -> string
*)

let hyoji ekimei = match ekimei with
  {kanji = kj; kana = kn; romaji = r; shozoku = s} -> s ^ ", " ^ kj ^ "(" ^ kn ^ ")"

(*test case
let test1 = hyoji {kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線"}
          = "丸ノ内線, 茗荷谷(みょうがだに)"
*)

(*romaji_to_kanji*)
(*
purpose:
  outline: receive romaji and ekimei_t list, return the kanji responsive to romaji
  header: romaji_to_kanji: ekimei_t list -> string
*)

let rec romaji_to_kanji roma lst = match lst with
  |  [] -> ""
  |  {kanji = kanji; kana = kana; romaji = romaji; shozoku = shozoku} :: rest
      -> if roma = romaji then kanji
         else romaji_to_kanji roma rest
(*test
let test1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test2 = romaji_to_kanji "" global_ekimei_list = ""
*)

(*get_ekikan_kyori*)
(*
purpose:
  outline: receive two string(ekimei) and return the span of them
           if the two stations are not related directly, return infinity.
  header: get_ekikan_kyori: string -> string -> ekikan_t list -> float
*)

let rec get_ekikan_kyori eki1 eki2 lst = match lst with
    [] -> infinity
  | {kiten = kiten; shuten = shuten; keiyu = keiyu; kyori = kyori; jikan = jikan;} :: rest
      -> if(eki1 = kiten && eki2 = shuten) || (eki1 = shuten && eki2 = kiten) then kyori
         else get_ekikan_kyori eki1 eki2 rest 

let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2
let test2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2