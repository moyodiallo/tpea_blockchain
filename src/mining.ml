open Letter

let shuffle_word (letters:letter list) limit =
  let rec without l i  =
    match l with
    | []    -> []
    | a::tl -> if i=0 then tl else a::without tl (i-1)
  in 
  let rec shuffle l limit len = 
    if l = [] || limit = 0 then []
    else  
      let i = Random.int (len) in 
        (List.nth l i)::shuffle (without l i) (limit-1) (len-1)
  in shuffle letters limit (List.length letters)

let verify_string_in_dictionary word_string dict_words = 
  let len = String.length word_string in 
  if len >= 1  && len <=10 then
    List.mem word_string (List.nth dict_words 0)
  else if len >=5 && len <= 15 then 
    List.mem word_string (List.nth dict_words 1)
  else if len >=25 && len <= 75 then
    List.mem word_string (List.nth dict_words 2)
  else if len >=50 && len <= 200 then
    List.mem word_string (List.nth dict_words 3)
  else 
    false


let verify_word_in_dictionary (letters: letter list) dict_words =
  let word_string = 
    List.fold_left (fun s l-> (String.make 1 (l.letter))^s) "" letters
  in verify_string_in_dictionary word_string dict_words

  
let load_dict_words () =
  [Client_utils.list_of_dict "dict/dict_100000_1_10.txt"]
  @ [Client_utils.list_of_dict "dict/dict_100000_5_15.txt"]
  @ [Client_utils.list_of_dict "dict/dict_100000_25_75.txt"]
  @ [Client_utils.list_of_dict "dict/dict_100000_50_200"]