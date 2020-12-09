open Word
open Constants
open Letter

(* ignoring unused variables - to be removed *)
let _ = ignore genesis

(* end ignoring unused variables - to be removed *)

let letter_score ~letter : int =  (int_of_char letter.letter) mod 10

let word_score ~word = 
  List.fold_right 
    (fun letter score -> (letter_score ~letter) + score) word.word 0
  
let fitness st word = 
  let rec fitness hash score =
    let word  = Store.get_word st hash in
      if word.level = 0 then 
        score 
      else fitness (word.head) (score + (word_score ~word))
  in fitness (Word.hash word) 0

let choose word1  word2 st = 
  let word1,word2 = Option.get word1,Option.get word2 in
  let fit1,fit2 = fitness st word1,fitness st word2 in
    if fit1 > fit2 then 
      Some word1 
    else if fit1 < fit2 then
      Some word2 
    else if word1.signature < word2.signature then 
      Some word1
    else if word1.signature > word2.signature then 
      Some word2
    else  failwith "fail head"

let head ?level (st : Store.word_store) =
  Hashtbl.fold 
      (fun _ (word:Word.word) w ->
        if Option.get level != word.level then
          w 
        else if w = None then 
          Some(word) 
        else 
          choose (Some word) w st) 
      (Store.get_words_table st)
      None