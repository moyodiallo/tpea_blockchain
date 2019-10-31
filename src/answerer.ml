open Messages
open Utils

let broadcast ?except braodcastpool msg =
  Log.log_info "@[<v 2> Braodcasting %a@]@."
    Messages.pp_message msg;
  Pool.iter_p braodcastpool (fun (point,(conn:Netpool.conn)) ->
      if unopt_map ~default:true (fun p -> point != p) except then
        Messages.send msg conn.fd
      else
        Lwt.return_unit
    )

let check_letter ({ letter; period ; head; author; signature } as l) =
  let open Crypto in
  let msg =
    hash_to_bigstring @@
    hash_list
      [ Utils.bigstring_of_char letter ;
        bigstring_of_int period ;
        hash_to_bigstring head;
        pk_to_bigstring author] in
  if verify ~pk:author ~msg ~signature then
    true
  else begin
    Log.log_warn "Signature check failed for %a@." pp_letter l;
    false end

  
let check_word ({ word; head; politician; signature } as w :word) =
  let open Crypto in
  let head_bs = hash_to_bigstring head
  and politician_bs = pk_to_bigstring politician
  and word_bs = List.map letter_to_bigstring word in
  let msg =
    hash_to_bigstring @@
      hash_list @@
        word_bs @ [head_bs; politician_bs ] in
  if verify ~pk:politician ~msg ~signature then
    true
  else begin
    Log.log_warn "Signature check failed for %a@." pp_word w;
    false end


let log_unexpected_message msg =
  Log.log_warn "@[<v 2>Unexpected msg %a.@]@.Igonoring it.@."
    Messages.pp_message
    msg;
  Lwt.return_unit



let answer
      (st : Netpool.worker_state)
      (msg : (Messages.message,string)  result ) =
  match msg with
  | Error s -> Log.log_error "Error decoding input message: %s" s;
               Messages.send
                 (Messages.Inject_raw_op
                    (Bytes.unsafe_of_string s)) st.fd
  | Ok msg ->
  Log.log_info "Processing messages @[%a@]@." Messages.pp_message msg;
  match msg with
  | Register id ->
     Mempool.register st.mempoolos id ;
     let lettres = Mempool.gen_letters st.mempoolos id in
     Messages.send
       (Messages.Letters_bag lettres)
       st.fd
  | Listen ->
     Pool.add st.netpoolos.broadcastpoolos st.point {point=st.point ; fd=st.fd}
  | Stop_listen ->
     Pool.remove st.netpoolos.broadcastpoolos st.point;
     Lwt.return_unit
  | Get_full_letterpool ->
     Messages.send
       (Messages.Full_letterpool (Mempool.letterpool st.mempoolos))
       st.fd

  | Get_full_wordpool ->
     Messages.send
       (Messages.Full_wordpool  (Mempool.wordpool  st.mempoolos))
       st.fd

  | Get_letterpool_since date ->
     Messages.send
       (Messages.Diff_letterpool
          {since = date ;
           letterpool = Mempool.letterpool_since st.mempoolos date } )
       st.fd

  | Get_wordpool_since date ->
     Messages.send
       (Messages.Diff_wordpool
          {since = date ;
           wordpool = Mempool.wordpool_since st.mempoolos date } )
       st.fd

  | Inject_letter l ->
     if (not st.check_sigs) || check_letter l then begin
         let next_turn, injected =  Mempool.inject_letter
                                      st.mempoolos
                                      l in
         let%lwt _bcst_msg = if injected then 
                              broadcast ~except:st.point st.netpoolos.broadcastpoolos msg
                            else Lwt.return_unit in
         match next_turn with
             Some p when Mempool.turn_by_turn st.mempoolos ->
                broadcast st.netpoolos.poolos
                  (Messages.Next_turn  p)
           | _ -> Log.log_info "Turn not complete @." ;
              Lwt.return_unit
       end else begin
         Log.log_warn "Injection failed for letter %a" pp_letter l;
         Lwt.return_unit
       end
  | Inject_word w ->
     if (not st.check_sigs) || check_word w then begin
         let next_turn, _injected =  Mempool.inject_word
                                       st.mempoolos
                                       w in 
         let%lwt _bcst_msg =
           broadcast ~except:st.point st.netpoolos.broadcastpoolos msg in
         begin match next_turn with
           Some p when Mempool.turn_by_turn st.mempoolos ->
            broadcast st.netpoolos.poolos
              (Messages.Next_turn  p)
         | _ -> Lwt.return_unit  end
       end else begin
         Log.log_warn "Injection failed for word %a" pp_word w;
         Lwt.return_unit
       end
  | Inject_raw_op _ ->
     broadcast ~except:st.point st.netpoolos.broadcastpoolos msg
  | Letters_bag _ -> log_unexpected_message msg
  | Next_turn _ -> log_unexpected_message msg
  | Full_letterpool _ -> log_unexpected_message msg
  | Diff_letterpool _ -> log_unexpected_message msg
  | Full_wordpool _ -> log_unexpected_message msg
  | Diff_wordpool _ -> log_unexpected_message msg
