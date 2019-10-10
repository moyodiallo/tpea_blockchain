open Messages

let json_pp ppf msg_yo =
  Yojson.Safe.pretty_print ~std:false ppf (message_to_yojson msg_yo)
  
let _ =
  Lwt_main.run begin
      let (pk,_sk) =  Hacl.Sign.keypair () in
      let pk_bytes = Bigstring.create Hacl.Sign.pkbytes in
      Hacl.Sign.blit_to_bytes pk pk_bytes;
      
      message_to_yojson Get_full_mempool
      |> Format.printf "json: '%a@'." (Yojson.Safe.pretty_print ~std:false) ;

      message_to_yojson Get_full_mempool
      |> Format.printf "yojson '%a'@."(Yojson.Safe.pp ) ;

      message_to_yojson (Register (pk_bytes))
      |> Format.printf "json: '%a'@."(Yojson.Safe.pretty_print ~std:false) ;

      
      let test_decoding msg =
        let _ = Format.printf "Testing %a@." json_pp msg in
        let%lwt f = Lwt_unix.openfile "test.test"
                      [Lwt_unix.O_WRONLY ;
                       Lwt_unix.O_TRUNC ; Lwt_unix.O_CREAT ] 0o666 in
        let%lwt _ = Format.printf "test opened@." ; Lwt.return_unit in
        let%lwt () = Messages.encode msg  f in
        let%lwt () = Lwt_unix.close f in
        let _ = Format.printf "re-open test@." in
        let%lwt f = Lwt_unix.openfile "test.test" [Lwt_unix.O_RDONLY] 0 in
        let%lwt maybe_message = Messages.decode f in
        let%lwt () = Lwt_unix.close f in
        match maybe_message with
          Ok message -> Lwt.return @@
                          if message = msg then
                            Format.printf "Decoding %a succesful@."  json_pp msg
                          else
                            Format.printf "@[<v 2>Decoding wrong value:@ \
                                           @[expected:%a @]@ \
                                           @[obtained:%a @]@]@."
                              json_pp msg
                              json_pp message
        | Error b -> Lwt.return @@ Format.printf "Error %s@." b
      in
      (* let%lwt () = *)
      test_decoding  (Register (pk_bytes))
      (* in
       * test_decoding  (Get_full_mempool)  *)
    end           
    
