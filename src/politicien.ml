(* open Messages *)
open Word
open Crypto
open Mempool   

type politician = { sk : Crypto.sk; pk : Crypto.pk } [@@deriving yojson, show]

type state = {
  politician : politician;
  word_store : Store.word_store;
  letter_store : Store.letter_store;
  next_words : word list;
}

let make_word_on_hash level letters politician head_hash : word =
  let head = head_hash in
  Word.make ~word:letters ~level ~pk:politician.pk ~sk:politician.sk ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = hash head in
  make_word_on_hash level letters politician head_hash
  
  
let send_new_word (state (pol, store, letters_s,_)) level =
  Option.iter
    (fun head ->
      let word = (* Création du mot *)
        (make_word_on_blockletters level letters pol head)
      in
      (* Récupération des lettres du letter store *)
      let letters =
        (Store.get_letters letters_s head)
      in
      (* Injection du mot *)
      let message = Messages.Inject_word word in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) store)



(* Vérification qu'il n'y a qu'une lettre soumise par auteur *) 
let check_valid_letter (letter_store (_, let_tables)) new_letter = 
      Hashtbl.fold(k, v, acc -> check_valid_letter_unit v new_letter acc) let_tables true


let check_valid_letter_unit (letter _ _ auth _) (letter _ _ auth2 _) acc = 
  | acc == false -> false
  | otherwise -> auth != auth2 
                
      
  
let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let pk,sk = Crypto.genkeys () in
  let reg_msg = Messages.register pk in
  Client_utils.send_some reg_msg;
  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;
  (*let politic = politician (sk,pk);*)
  
  (* Generate initial blocktree *)
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false
  in
  let store = Store.init_words () in
  Store.add_words store wordpool.words;
  (* Get initial letterpool *)
  let letterpool =
    match Client_utils.receive () with
    | Messages.Full_letterpool letterpool -> letterpool
    | _ -> assert false
  in
  (* Generate initial letterpool *)
  let store_letter = Store.init_letters () in
  Store.add_letter store_letter letterpool.letters;
  let politic = politician sk pk
  let st = State(politic, store, store_letter,[] )
  in send_new_word st level;
  
  
     
  (* Create and send first word *)
  send_new_word st 0;
  (* start listening to server messages *)
  Client_utils.send_some Messages.Listen;
  (*  main loop *)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
        (* Injection d'une lettre dans le serveur*)
        | Messages.Inject_letter l ->
      (*Ajout de la lettre dans le store des lettres *)
           Store.add_letter store_letter l;
           (*  *) 
           Option.iter
           
           
             
           
    )
  

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) ()
  in
  main
