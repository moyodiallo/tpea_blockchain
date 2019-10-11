(* open Error *)

type point  =  {addr : Unix.inet_addr ;
                port : int}
type conn =  { point : point ;
              fd : Lwt_unix.file_descr ;
            }
type pool =
  { poolos : (point,conn) Pool.t;
    default_callback : state -> (Messages.message, string) result  -> unit Lwt.t }

and state = {
    pool : pool ;
    fd : Lwt_unix.file_descr ;
    callback : state -> (Messages.message, string) result  -> unit Lwt.t
  }
        
let rec worker_loop (st:state) =
  let%lwt () = Lwt_unix.yield () in
  let%lwt message =
    let () = Format.printf "Waiting for messages.@." in
    Messages.receive st.fd in
  let%lwt () = st.callback st message  in
  let () = Format.printf "Message processed.@." in
  worker_loop st

let pp_may_message ppf may_message =
  match may_message with
    Ok msg ->  Format.fprintf ppf "message :%a" Messages.pp_message msg
  | Error s -> Format.fprintf ppf "Error %s" s
  
let create () =
  {poolos = Pool.create ();
   default_callback =
     fun _ msg ->
     Format.printf
       "Received : %a@."
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
    Format.printf "STARTING: Answering loop started for %a@."
    pp_point (addr,port)
    ;
    worker_loop {fd;
                 callback =
                   Option.value callback
                     ~default:pool.default_callback;
                 pool}
    end
    begin fun _exn ->
    let _ = Format.printf "STOPPING: answerer for %a@."
              pp_point (addr,port) in
    Pool.remove pool.poolos {addr;port}; Lwt.return_unit end
