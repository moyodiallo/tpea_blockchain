let conn : Lwt_unix.file_descr option ref = ref None

let connect  ?(addr = Unix.inet_addr_of_string "::1" ) ?(port=12345) () =
  let main_socket = Lwt_unix.(socket PF_INET6 SOCK_STREAM 0) in
  let remote_addr =  Lwt_unix.ADDR_INET (addr, port) in
  let%lwt () =  Lwt_unix.connect main_socket remote_addr in
  conn := Some main_socket; Lwt.return_unit


let send msg =
  Option.map
    (Messages.send msg)
    !conn

let receive ?check () =
  Option.map
    (Messages.receive)
    !conn |>
    function  None -> Log.log_error "No connection@";
                      Lwt.return_unit
            | Some t -> let%lwt t = t in
                        begin match t with
                        | Ok msg ->
                           Option.iter (fun f -> f  msg) check;
                           Log.log_info "Received %a@."
                             Messages.pp_message msg
                        | Error s ->
                           Log.log_error "Reception error %s@." s
                        end; Lwt.return_unit


exception Test_failure of (string*string)

let checker ~hard test sent received =
  if test (sent, received) then
    Log.log_success
      "message %a after %a@."
      Messages.pp_message received
      Messages.pp_message sent
  else begin
      Log.log_error
        "message %a after %a@."
        Messages.pp_message received
        Messages.pp_message sent;
      if hard then
        raise @@ Test_failure
          (Messages.show_message sent ,
           Messages.show_message received)
    end

let check_register ~hard =
  checker ~hard
    (function (Register _ ,Letters_bag _) -> true
            | _ -> false)
let check_get_full_letterpool ~hard =
  checker ~hard
    (function (Get_full_letterpool ,Full_letterpool _) -> true
            | _ -> false)
let check_get_letterpool_since ~hard =
  checker ~hard
    (function (Get_letterpool_since d ,Diff_letterpool {since;_} ) when d=since -> true
            | _ -> false)

let check_get_full_wordpool ~hard =
  checker ~hard
    (function (Get_full_wordpool ,Full_wordpool _) -> true
            | _ -> false)
let check_get_wordpool_since ~hard =
  checker ~hard
    (function (Get_wordpool_since d ,Diff_wordpool {since;_} ) when d=since -> true
            | _ -> false)

let check_inject_letter ~hard =
  checker ~hard
    (function (Inject_letter l ,Diff_letterpool {letterpool;_})
              when List.exists (fun (_,l1) -> l=l1) letterpool.letters -> true
            | _ -> false)
let check_inject_word ~hard =
  checker ~hard
    (function (Inject_word w ,Diff_wordpool {wordpool;_})
              when List.exists (fun (_,w1) -> w=w1) wordpool.words -> true
            | _ -> false)

let no_some v =
  match v with None -> failwith "No connection"
              |Some v -> v
let send_some v =
  Log.log_info "Sending %a@."
    Messages.pp_message v;
  no_some @@ send v
let test ?(hard=false) () =
  let (pk,sk) =  Crypto.genkeys () in


  let register  =(Messages.Register pk) in
  let%lwt () = send_some register  in
  let%lwt () = receive ~check:(check_register ~hard register) () in

  let get_full_letterpool  =(Messages.Get_full_letterpool) in
  let%lwt () = send_some get_full_letterpool  in
  let%lwt () = receive ~check:(check_get_full_letterpool ~hard get_full_letterpool) () in

  let get_full_wordpool  =(Messages.Get_full_wordpool) in
  let%lwt () = send_some get_full_wordpool  in
  let%lwt () = receive ~check:(check_get_full_wordpool ~hard get_full_wordpool) () in

  let bag = Author.{author = pk ; sk; content=['a';'b']} in
  let letter = Author.make_letter_on_hash bag 0 Author.genesis 'a' in
  let message = Messages.Inject_letter letter in
  let%lwt () = send_some message in
  let getpool = Messages.Get_letterpool_since 0 in
  let%lwt () = send_some getpool in
  let%lwt () = receive ~check:(check_inject_letter ~hard message) () in
Lwt.return_unit
