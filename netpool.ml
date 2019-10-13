(* open Error *)

type point  =  {addr : Unix.inet_addr ;
                port : int}
type conn =  { point : point ;
              fd : Lwt_unix.file_descr ;
            }
type pool =
  { poolos : (point,conn) Pool.t ;
    broadcastpoolos : (point,conn) Pool.t ;
    letterpoolos : Messages.letterpool ;
    wordpoolos : Messages.wordpool ;
    mutable current_period : int;
    default_callback : worker_state -> (Messages.message, string) result  -> unit Lwt.t }

and worker_state = {
    pool : pool ;
    point : point ;
    fd : Lwt_unix.file_descr ;
    callback : worker_state -> (Messages.message, string) result  -> unit Lwt.t
  }

let rec worker_loop (st:worker_state) =
  let%lwt () = Lwt_unix.yield () in
  let%lwt message =
    let () = Log.log_info "Waiting for messages.@." in
    Messages.receive ~verbose:true st.fd in
  let%lwt () = st.callback st message  in
  let () = Log.log_info "Message processed.@." in
  worker_loop st

let pp_may_message ppf may_message =
  match may_message with
    Ok msg ->  Format.fprintf ppf "message :%a" Messages.pp_message msg
  | Error s -> Format.fprintf ppf "Error %s" s

let create () =
  let broadcastpoolos  = Pool.create () in
  {current_period =0;
    broadcastpoolos;
    poolos = Pool.create
               ~remove_callback:(fun (point,_) _ ->
                 Pool.remove broadcastpoolos point; Lwt.return_unit )
               ();
   letterpoolos = Messages.init_letterpool;
   wordpoolos = Messages.init_wordpool;
   default_callback =
     fun _ msg ->
     Log.log_info
       "@[<v 2>Received: %a@]@."
       pp_may_message
       msg;
     Lwt.return_unit
  }

let pp_point ppf (addr,port) =
  Format.fprintf ppf "%s:%i"
    (Unix.string_of_inet_addr addr)
    port

let accept (pool:pool) ?callback fd (addr,port) =
  let point = {addr;port} in
  let%lwt () =  Pool.add pool.poolos point {point;fd} in
  Lwt.catch begin fun () ->
    Log.log_info "STARTING: Answering loop started for %a@."
    pp_point (addr,port)
    ;
      worker_loop {point;
                   fd;
                 callback =
                   Option.value callback
                     ~default:pool.default_callback;
                 pool}
    end
    begin fun _exn ->
    let _ = Log.log_info "STOPPING: answerer for %a@."
              pp_point (addr,port) in
    Pool.remove pool.poolos {addr;port}; Lwt.return_unit end
