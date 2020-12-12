(* open Messages *)
open Word
open Crypto
open Letter 

type politician = { sk : Crypto.sk; pk : Crypto.pk } [@@deriving yojson, show]

type state = {
  politician : politician;
  word_store : Store.word_store;
  letter_store : Store.letter_store;
  next_words : word list;
}

(* Loading dictionaries *)
let dict_words = Mining.load_dict_words ()

let make_word_on_hash level letters politician head_hash : word =
  let head = head_hash in
  Word.make ~word:letters ~level ~pk:politician.pk ~sk:politician.sk ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = hash head in
  make_word_on_hash level letters politician head_hash

let rec generate_word letters times =
  if letters = [] || times = 0 then 
    None
  else
    let m = Mining.shuffle_word letters (1 + Random.int (List.length letters)) in
      if Mining.verify_word_in_dictionary m dict_words then
        Some m 
      else 
        generate_word letters (times-1)

let send_new_word st_letter st_word level politician = 
  let letters = List.of_seq (Hashtbl.to_seq (Store.get_letters_table st_letter)) in
  let letters = List.map (fun (_,l) -> l) letters in
  let letters = List.filter (fun l -> l.level = level) letters in

  (* le mot generer par le policticien "liste de lettre"*)
  let letters = generate_word letters (-1) in 
  if letters = None then 
    ()
  else
  Option.iter
    (fun head ->
      let word = (* Création du mot *)
        make_word_on_blockletters 
          level 
          (Option.get letters) 
          politician 
          (Word.to_bigstring head)
      in
      (* Injection du mot *)
      let message = Messages.Inject_word word in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) st_word)
  
let run ?(max_iter = 0) () =

  (* Generate public/secret keys *)
  let pk,sk = Crypto.genkeys () in

  (*the politician*)
  let politician = {sk;pk} in 

  (*Register to the server *)
  let reg_msg = Messages.Register pk in
  Client_utils.send_some reg_msg;

  (* drop provided letter_bag *)
  ( match Client_utils.receive () with
  | Messages.Letters_bag _ -> ()
  | _ -> assert false ) ;

  
  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;
  
  (* Generate initial blocktree *)
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false
  in

  (* Generate initial blocktree *)
  let store_word = Store.init_words () in
  Store.add_words store_word wordpool.words;

  (*Get initial letterpool*)
  let getpool = Messages.Get_full_letterpool in 
  Client_utils.send_some getpool;

  (* Get initial letterpool *)
  let letterpool =
    match Client_utils.receive () with
    | Messages.Full_letterpool letterpool -> letterpool
    | _ -> assert false
  in

  (* Generate initial letterpool *)
  let store_letter = Store.init_letters () in
  Store.add_letters store_letter letterpool.letters;
  
  (*Create and send the first word*)
  send_new_word store_letter store_word wordpool.current_period politician;

  (* start listening to server messages *)
  Client_utils.send_some Messages.Listen;

  (*  main loop *)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
      | Messages.Inject_word w ->
        Store.add_word store_word w ;
        Option.iter
          (fun head ->
            if head = w then (
              Log.log_info "Head updated to incoming word %a@." Word.pp w ;
              send_new_word store_letter store_word !level politician)
            else Log.log_info "incoming word %a not a new head@." Word.pp w)
          (Consensus.head ~level:(!level - 1) store_word)
          
        (* Injection d'une lettre dans le serveur *)
        | Messages.Inject_letter l ->    
          (*Ajout de la lettre dans le store des lettres *)
           Store.add_letter store_letter l;
           (* Ajout du mot *) 
            Option.iter
              (fun head ->
                  Log.log_info "Head of the chain %a@." Word.pp head ;
                  send_new_word store_letter store_word !level politician )
              (Consensus.head ~level:(!level - 1) store_word)
        | Messages.Next_turn p -> level :=p; Log.log_info "Next Turn"
        | _ -> () ) ;
      loop (max_iter - 1) )
  in
  loop max_iter

let _ =
  let main =
    Log.log_info "politician";
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) ()
  in
  main