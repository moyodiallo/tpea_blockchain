
let _ =
  let addr_ref = ref None in
  let port_ref = ref 12345 in
  let no_turn_ref = ref true  in
  let timeout_ref = ref None in
  let parse_list = [("-bind",
                     Arg.String (fun s -> addr_ref := Some (Unix.inet_addr_of_string s)),
                     ":Address to which the server should bind (default is any)") ;
                    ("-port",
                     Arg.Set_int port_ref,
                     ":Port to which the server should bind (default is any)") ;
                    ("-no-turn",
                     Arg.Set no_turn_ref,
                     ":Disable the turn-by-turn mechanism") ;
                    ("-timeout",
                     Arg.Float (fun t -> timeout_ref := Some t),
                     "Turns last no longer than [t].\n    \
                      This option has no effect if the turn-by-turn mechanism is not activated.")
                   ] in
  let doc =  "Usage: server [options]" in
  Arg.parse parse_list (fun _doc -> failwith "Unexpected argument")
    doc;
  let serve =
    Server.serve
      ?addr:!addr_ref
      ~port:!port_ref
      ~turn_by_turn:(not !no_turn_ref)
      ?timeout:!timeout_ref
      () in
  Lwt_main.run serve
