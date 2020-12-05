open Word
open Constants

(* ignoring unused variables - to be removed *)
let _ = ignore genesis

(* end ignoring unused variables - to be removed *)

let word_score word = 
  List.fold_right ((letter,acc) -> letter_score letter acc  ) 0 word.word

let letter_score (letter l _ _ _ _) acc =
  match l with
  | 'a' -> acc + 1
  | 'b' -> acc + 3
  | 'c' -> acc + 3
  | 'd' -> acc + 2
  | 'e' -> acc + 1
  | 'f' -> acc + 4
  | 'g' -> acc + 2
  | 'h' -> acc + 4
  | 'i' -> acc + 1
  | 'j' -> acc + 8
  | 'k' -> acc + 5
  | 'l' -> acc + 1
  | 'm' -> acc + 3
  | 'n' -> acc + 1
  | 'o' -> acc + 1
  | 'p' -> acc + 3
  | 'q' -> acc + 10
  | 'r' -> acc + 1
  | 's' -> acc + 1
  | 't' -> acc + 1 
  | 'u' -> acc + 1
  | 'v' -> acc + 4
  | 'w' -> acc + 4
  | 'x' -> acc + 8
  | 'y' -> acc + 4
  | 'z' -> acc + 10
    
  
let fitness st word = word_score word


let head ?level (st : Store.word_store) =
  (* récupère les mots ayant un level inférieur ou égal au level du paramètre *)
  let word, _ = List.fold_right ( (w, score) (best_word, best_score) -> if score >= best_score then (w,score) else (best_word, best_score)) (None,0)
    ((List.map (w -> (w, fitness st w)) (* construction list de tuples (mot, score) *)
        (List.of_seq (* list *)
           (Seq.filter (word -> w.level >= level ) (* filtre des mots en fonction de leur level *)
              (Hashtbl.to_seq_values st.words_table)))) (* table de mots en sequences de mots*)
    )
  in word
