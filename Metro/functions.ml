#use "metro.ml"

let list1 = 
  [
    {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"};
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}
  ]
let list2 = []

let list3 =
  [
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"};
    {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
  ]


 
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
(*
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2
let test2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2
*)

(*kyori_wo_hyoji
purpose:
  outline: receive two strings(romaji ekimei) and return strings
                                                          if the two ekis are related directly"A駅からB駅までは{int}kmです"
                                                          else "A駅とB駅はつながっていません"
  header: kyori_wo_hyoji: string -> string -> string
*)

let kyori_wo_hyoji roma_eki1 roma_eki2 = 
  let kanji_eki1 = romaji_to_kanji roma_eki1 global_ekimei_list in 
  let kanji_eki2 = romaji_to_kanji roma_eki2 global_ekimei_list in 
  let kyori = get_ekikan_kyori kanji_eki1 kanji_eki2 global_ekikan_list in
    if kanji_eki1 = "" then if kanji_eki2 = "" then  roma_eki1 ^ "と" ^ roma_eki2 ^ "という駅は存在しません"
                            else roma_eki1 ^ "という駅は存在しません" 
    else if kanji_eki2 = "" then roma_eki2 ^ "という駅は存在しません"    
    else if(kyori = infinity) then kanji_eki1 ^ "と" ^ kanji_eki2 ^ "はつながっていません"
    else kanji_eki1 ^ "から" ^ kanji_eki2 ^ "までは" ^ string_of_float(kyori) ^ "kmです"

(*
let test1 = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷から新大塚までは1.2kmです"
let test2 = kyori_wo_hyoji "eidanakatsuka" "kagurazaka" = "営団赤塚と神楽坂はつながっていません"
let test3 = kyori_wo_hyoji "pakipaki" "puripuri" = "pakipakiとpuripuriという駅は存在しません"
let test4 = kyori_wo_hyoji "myogadani" "puripuri" = "puripuriという駅は存在しません"
let test5 = kyori_wo_hyoji "pakipaki" "myogadani" = "pakipakiという駅は存在しません"
*)

(*
make_eki_list:
    outline: receive ekimei_t list and return eki_t list made from the received list
    type: ekimei_t list -> eki_t list
*)


let rec make_eki_list lst = List.map (fun {kanji = k} -> {namae = k; saitan_kyori = infinity; temae_list = []}) lst

let test1 = make_eki_list list1 = 
  [{namae = "代々木上原"; saitan_kyori = infinity; temae_list = []};
   {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []};
   {namae = "明治神宮前"; saitan_kyori = infinity; temae_list = []};
   {namae = "明治神宮前"; saitan_kyori = infinity; temae_list = []}
  ]
let test2 = make_eki_list list2 = []

(*
shokika:
  outline: receive eki_t list and string(kiten, kanji), return eki_t list that the first item's saitan_kyori <- 0 and temae_list <- first
  type: eki_t list -> string -> eki_t list
*)
let rec shokika lst kiten = match lst with 
    [] -> []
  | {namae = n; saitan_kyori = k; temae_list = t} as first :: rest ->
      if n = kiten
        then {namae = n; saitan_kyori = 0.; temae_list = [n]} :: rest
        else first :: shokika rest kiten
(*let test1 = shokika (make_eki_list list1) "代々木上原" = 
  [{namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]};
   {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []};
   {namae = "明治神宮前"; saitan_kyori = infinity; temae_list = []};
  ]
*)

(*
seiretsu:
  outline: receive ekimei_t list and return the received list sorted by hiragana order and remove duplicate stations
  type: ekimei_t list -> ekimei_t list
*)

let rec sort eki eki_lst = match eki_lst with
  [] -> [eki]
| {kana = k} as first :: rest -> match eki with {kana = eki_k} ->
  if eki_k < k
    then eki :: first :: rest
  else if eki_k = k
    then first :: rest
  else first :: sort eki rest

let rec seiretsu lst = match lst with
    [] -> []
  | first :: rest -> sort first (seiretsu rest)

(*let test1 = seiretsu list1 = [
  {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"};
  {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
  {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]*)

(*
koushin1:
  outline: receive station(eki_t) p and q, return update q if p & q are related directly or just q
  type: eki_t -> eki_t -> eki_t
*)



(*
let test1 = 
  koushin1 {namae = "代々木上原"; saitan_kyori = 1.2; temae_list = ["代々木上原"]} {namae = "代々木公園"; saitan_kyori = 3.; temae_list = []}
    = {namae = "代々木公園"; saitan_kyori = 2.2; temae_list = ["代々木公園"; "代々木上原"; ]}

let test2 = 
  koushin1 {namae = "代々木上原"; saitan_kyori = 1.2; temae_list = ["代々木上原"]} {namae = "たまご"; saitan_kyori = 3.; temae_list = []}
    = {namae = "たまご"; saitan_kyori = 3.; temae_list = []}
*)

(*
koushin:
  outline: receive the previous station(eki_t) and v()
  type: eki_t -> eki_t list -> eki_t list
*)
let koushin p v = 
  let koushin1 p q = 
    match p with {namae = n_p; saitan_kyori = s_p; temae_list = t_p} ->
    match q with {namae = n_q; saitan_kyori = s_q; temae_list = t_q} ->
      let kyori = get_ekikan_kyori n_p n_q global_ekikan_list in
        if(kyori < s_q -. s_p)
          then {namae = n_q; saitan_kyori = s_p +. kyori; temae_list = n_q :: t_p}
        else q
    in
      List.map (koushin1 p) v

let test1 = 
  koushin {namae = "代々木公園"; saitan_kyori = 0.; temae_list = ["代々木公園"]} [{namae = "代々木上原"; saitan_kyori = infinity; temae_list = []}; {namae="明治神宮前"; saitan_kyori= infinity; temae_list = []}]
  = [{namae = "代々木上原"; saitan_kyori = 1.0; temae_list = ["代々木上原"; "代々木公園"]}; {namae="明治神宮前"; saitan_kyori= 1.2; temae_list = ["明治神宮前"; "代々木公園"]}]

