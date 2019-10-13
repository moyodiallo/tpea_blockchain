open Messages
open Utils

let gen_letters _id = ['a';'b'; Char.chr @@ Random.int (255)]


let strip_letterpool date {period;letters} =
  {period ;
   letters = List.filter (fun (d,_) -> d>= date) letters}

let strip_wordpool date {period;words} =
  {period ;
   words = List.filter (fun (d,_) -> d>= date) words}

let broadcast ?except braodcastpool msg =
  Pool.iter_p braodcastpool (fun (point,(conn:Netpool.conn)) ->
      if unopt_map ~default:true (fun p -> point != p) except then
        Messages.send msg conn.fd
      else
        Lwt.return_unit
    )

let check_letter pool ({ letter; head; author; signature } as l) =
  let open Crypto in
  let msg =
    hash_to_bigstring @@
    hash_list
    [ Utils.bigstring_of_char letter ;
     hash_to_bigstring head;
     pk_to_bigstring author] in
  if verify ~pk:author ~msg ~signature then
       (* if well signed, check if not already injected  *)
    Messages.find_by_author
      pool.Netpool.letterpoolos
      ~period:((=)pool.current_period)
      author |> is_nil
  else begin
    Log.log_warn "Signature check failed for %a@." pp_letter l;
    false end





let answer (st : Netpool.worker_state) (msg :
                                          (Messages.message,string)
                                            result ) =
  match msg with
  | Error s -> Log.log_error "Error decoding input message: %s" s;
               Messages.send
                 (Messages.Inject_raw_op
                    (Bytes.unsafe_of_string s)) st.fd
  | Ok msg ->
  Log.log_info "Processing messages @[%a@]@." Messages.pp_message msg;
  match msg with
  | Register id ->
     let lettres = gen_letters id in
     Messages.send
       (Messages.Letters_bag lettres)
       st.fd
  | Listen ->
     Pool.add st.pool.broadcastpoolos st.point {point=st.point ; fd=st.fd}
  | Stop_listen ->
     Pool.remove st.pool.broadcastpoolos st.point;
     Lwt.return_unit
  | Get_full_letterpool ->
     Messages.send
       (Messages.Full_letterpool st.pool.letterpoolos)
       st.fd

  | Get_full_wordpool ->
     Messages.send
       (Messages.Full_wordpool st.pool.wordpoolos)
       st.fd

  | Get_letterpool_since date ->
     Messages.send
       (Messages.Diff_letterpool
          {since = date ;
           letterpool = strip_letterpool date st.pool.letterpoolos } )
       st.fd

  | Get_wordpool_since date ->
     Messages.send
       (Messages.Diff_wordpool
          {since = date ;
           wordpool = strip_wordpool date st.pool.wordpoolos } )
       st.fd

  | Inject_letter l ->
     if check_letter st.pool l then begin
       add_letter st.pool.letterpoolos
         st.pool.current_period
         l;
       broadcast ~except:st.point st.pool.broadcastpoolos msg
     end else begin
       Log.log_warn "Injection failed for letter %a" pp_letter l;
       Lwt.return_unit
       end
  | Inject_word w ->
     add_word st.pool.wordpoolos
       st.pool.current_period
       w;
     broadcast ~except:st.point st.pool.broadcastpoolos msg
  | Inject_raw_op _ ->
     broadcast ~except:st.point st.pool.broadcastpoolos msg
  | Letters_bag _ -> (assert false)
  | Full_letterpool _ -> (assert false)
  | Diff_letterpool _ -> (assert false)
  | Full_wordpool _ -> (assert false)
  | Diff_wordpool _ -> (assert false)
