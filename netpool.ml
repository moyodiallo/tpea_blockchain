
type 'messages point  =  {addr : Unix.inet_addr ;
                port : int ;
                fd : Lwt_unix.file_descr option ;
                answerer : 'messages Answerer.t
               }
type 'messages pool =
  {pool : 'messages point Pool.t }
  

let accept pool fd (addr,port) =
  let answerer = Answerer.create pool fd in
  pool.add {addr;port;fd;answerer};
  answerer.run ()
  

                                                                  
