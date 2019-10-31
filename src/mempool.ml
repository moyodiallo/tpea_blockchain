open Messages

type internal_letterpool = Messages.letterpool

type internal_wordpool =  Messages.wordpool

type mempool =
  { turn_by_turn : bool ;
    nb_rounds : int ;
    timeout : float option ;
    mutable registered : Messages.author_id list ;
    letterpoolos : internal_letterpool ;
    wordpoolos : internal_wordpool ;
    mutable current_period : int;
    mutable current_period_start : float;
  }

let period_of_int i = i

(* let current_period {current_period; _ } =
 *   current_period *)
(* let period_to_int p = p *)

(** {1} Letters pool *)
let add_letter pool  letter =
  let key = pool.current_period in
  pool.letterpoolos.letters <- (key ,letter)::pool.letterpoolos.letters

(* let remove_letter pool letter =
 *   pool.letterpoolos.letters <-
 *     (List.filter (fun (_,l) -> l != letter) pool.letterpoolos.letters) *)

(* let find_by_author (pool:mempool) ?period author  =
 *   List.filter
 *     (fun (p,(l:letter)) -> Option.value ~default:(fun _ -> true) period p
 *                            && l.author = author)
 *     pool.letterpoolos.letters *)

let init_letterpool = {current_period=period_of_int 0;next_period=period_of_int 1; letters = []}

let letterpool_since { letterpoolos;_} since =
  {letterpoolos with
    letters = List.filter (fun (p,_) -> p>= since) letterpoolos.letters}

let letterpool pool = pool.letterpoolos

(** {1} Words pool *)
let add_word pool word =
  pool.wordpoolos.words <-
    (pool.current_period,word)::pool.wordpoolos.words;
  true

let init_wordpool = {current_period=period_of_int 0;
                     next_period=period_of_int 1;
                     words = []}

let wordpool_since {wordpoolos; _ } since =
  { wordpoolos with
    words = List.filter (fun (p,_) -> p>= since) wordpoolos.words}

let wordpool pool = pool.wordpoolos

(** {1} Mempool  *)

let create ~turn_by_turn ?(nb_rounds=100) ?timeout () =
  let period = 0
  and current_period_start = Unix.time ()
  in
  { turn_by_turn ;
    nb_rounds ;
    timeout ;
    registered =[];
    letterpoolos =  init_letterpool;
    wordpoolos = init_wordpool;
    current_period = period ;
    current_period_start
  }

let turn_by_turn {turn_by_turn ; _} = turn_by_turn
                                    
let gen_letters mempool _id =
  let code_z = 122 in
  let code_a = 97 in
  let rec aux round res =
    if round >0 then
      aux (round - 1)  ((Char.chr @@ (Random.int (code_z-code_a))+code_a) ::res)
    else res
  in aux mempool.nb_rounds []

let register pool id = pool.registered <-
                         id::(Utils.remove_first pool.registered id)

let next_period pool =
  if not pool.turn_by_turn then
      Some 0
  else
    (* get author that have injected at this level *)
    let injecters =
      List.filter (fun (p,_) -> p = pool.current_period)
        pool.letterpoolos.letters |>
        List.map (fun (_,{ author; _ })-> author)

    in
    Log.log_info "@[<v 2> Injecters at level %d: @[ %a@] Registered :@[ %a@] @]@."
      pool.current_period 
      (Format.pp_print_list Messages.pp_author_id)
      injecters
      (Format.pp_print_list Messages.pp_author_id)
      pool.registered;
    if Utils.included pool.registered injecters
       || (Utils.unopt_map
             (fun tio -> Unix.time () > pool.current_period_start
                                        +. tio)
             ~default:false
             pool.timeout
          )
    then begin
        pool.current_period <- pool.current_period + 1 ;
        let current_period = period_of_int pool.current_period
        and next_period = period_of_int (pool.current_period + 1) in
        pool.wordpoolos.current_period <- current_period ;
        pool.wordpoolos.next_period <- next_period ;
        pool.letterpoolos.current_period <- current_period;
        pool.letterpoolos.next_period <- next_period;
        pool.current_period_start <- Unix.time();
        Some current_period
      end
    else None

let inject_letter
      (pool:mempool)
      ({  period;_ } as l:letter) =
  if
    not pool.turn_by_turn
    || period = pool.letterpoolos.current_period
  then begin
      add_letter pool l;
      let period_change = next_period pool in
      (period_change, true)
    end
  else begin
      let open Messages in
      Log.log_warn
        "Out of timeframe letter %a (current: %a, next: %a)"
        pp_letter l
        pp_period pool.letterpoolos.current_period
        pp_period pool.letterpoolos.next_period ;
      (None, false) end

let inject_word
      (pool:mempool)
      (w:word) =
  let period_change = next_period pool in
  (period_change, add_word pool w)
