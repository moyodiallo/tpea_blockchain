type 'data t =
  {mutable data : 'data list;
   add_callback : 'data -> 'data t -> unit Lwt.t;
   remove_callback : 'data -> 'data t -> unit Lwt.t
  }

let add pool data =
  pool.data <- data::pool.data;
  pool.add_callback data pool

let removepool pool data =
  pool.data <- List.filter (fun d -> d <> data) pool.data;
  pool.remove_callback data pool

let map pool f =
  List.map f pool.data
                       

                                                                  
