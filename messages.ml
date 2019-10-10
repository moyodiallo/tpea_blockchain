type hash = bytes [@@deriving yojson]
type signature = bytes [@@deriving yojson]
type pkh = hash [@@deriving yojson]
(* type pk = bytes  [@@deriving yojson] *)

type pk = Bigstring.t 
let pk_to_yojson pk = 
    [%to_yojson: string]
    @@ (Format.asprintf "%a" Hex.pp)
    @@ Hex.of_bigstring  pk

let (>>?) b a =
  match b with
    Ok b -> Ok (a b)
  | Error _ as err-> err

let hex_of_hexstring str : Hex.t=
  `Hex str

                   
let pk_of_yojson pkj =
   ( [%of_yojson: string] pkj) >>?
     hex_of_hexstring  >>?
     Hex.to_bigstring

      
type period = int [@@deriving yojson]

type author_id = pk  [@@deriving yojson]

type politician_id = pk  [@@deriving yojson]

               
type letter =
  { letter : char ;
    head : hash ;
    id : author_id ;
    signature : signature
  }
    [@@deriving yojson]

type word = {
    word : letter list ;
    head : hash ;
    id : politician_id ;
    signature : signature
  } [@@deriving yojson]
    
type mempool =
  {period : int ; letters : letter list } [@@deriving yojson]

type diff_mempool_arg = {since : period; mempool : mempool}[@@deriving yojson]
  
type message =
  | Register of author_id
  | Full_mempool of mempool
  | Diff_mempool of diff_mempool_arg
  | Get_full_mempool
  | Get_mempool_since of period
  | Inject_letter of letter
  | Inject_word of word
  | Inject_raw_op of bytes

type register_msg = { register : author_id}[@@deriving yojson]
type full_mempool_msg = { full_mempool : mempool}[@@deriving yojson]
type diff_mempool_msg = { diff_mempool : diff_mempool_arg }[@@deriving yojson]
type get_mempool_since_msg = { get_mempool_since : period}[@@deriving yojson]
type get_full_mempool_msg = { get_full_mempool : unit }[@@deriving yojson]
type inject_letter_msg = { inject_letter : letter}[@@deriving yojson]
type inject_word_msg = { inject_word : word}[@@deriving yojson]
type inject_raw_op_msg = { inject_raw_op : bytes}[@@deriving yojson]

let register_to_message { register = x } =  Register x
let full_mempool_to_message { full_mempool = x } =  Full_mempool x
let diff_mempool_to_message { diff_mempool = x } =  Diff_mempool x
let get_mempool_since_to_message { get_mempool_since = x } =  Get_mempool_since x
let get_full_mempool_to_message { get_full_mempool = () } =  Get_full_mempool
let inject_letter_to_message { inject_letter = x } =  Inject_letter x
let inject_word_to_message { inject_word = x } =  Inject_word x
let inject_raw_op_to_message { inject_raw_op = x } =  Inject_raw_op x

                       
let message_to_yojson msg =
  match msg with
  | Register x -> [%to_yojson: register_msg] { register = x}
  | Full_mempool x -> [%to_yojson: full_mempool_msg] { full_mempool = x}
  | Diff_mempool x -> let x = { diff_mempool = x} in[%to_yojson: diff_mempool_msg] x
  | Get_full_mempool -> [%to_yojson: get_full_mempool_msg] { get_full_mempool = ()}
  | Get_mempool_since x -> [%to_yojson: get_mempool_since_msg] { get_mempool_since = x}
  | Inject_letter x -> [%to_yojson: inject_letter_msg] { inject_letter = x}
  | Inject_word x -> [%to_yojson: inject_word_msg] { inject_word = x}
  | Inject_raw_op x -> [%to_yojson: inject_raw_op_msg] { inject_raw_op = x}

let (||?) (i,r) f =
  match r with
    Ok _ as v -> (i,v)
  | Error _  -> (i, f i)

let (>|>)  f g = fun x -> (f x >>? g)

let message_of_yojson msg =
  (msg, Error "")
  ||?  ([%of_yojson: register_msg] >|> register_to_message)
  ||? ([%of_yojson: full_mempool_msg] >|> full_mempool_to_message)
  ||? ([%of_yojson: diff_mempool_msg] >|> diff_mempool_to_message)
  ||? ([%of_yojson: get_full_mempool_msg] >|> get_full_mempool_to_message)
  ||? ([%of_yojson: get_mempool_since_msg] >|> get_mempool_since_to_message)
  ||? ([%of_yojson: inject_letter_msg] >|> inject_letter_to_message)
  ||? ([%of_yojson: inject_word_msg] >|> inject_word_to_message)
  ||? ([%of_yojson: inject_raw_op_msg] >|> inject_raw_op_to_message)
  |>snd 
let size_of_length = 8

(* let byte_of_length len =
 *   let buf = Bytes.create size_of_length in
 *   Bytes.set_int64_le buf 0 (Int64.of_int len);
 *   buf *)

exception Writing_failure 

let to_write_size = Bytes.create size_of_length

let write_channel ch buf ofs length =
  let%lwt written = Lwt_unix.write ch buf ofs length
  in if written < length then
       raise Writing_failure
     else
       Lwt.return_unit
        

let encode msg out_ch =
  let str = message_to_yojson msg |> Yojson.Safe.to_string |> Bytes.unsafe_of_string in
  let len = Bytes.length str in
  let () = Int64.of_int len |> Bytes.set_int64_le to_write_size 0 in
  let%lwt () = write_channel out_ch to_write_size 0 size_of_length in
  write_channel out_ch str 0 len 

let rcv_size = Bytes.create size_of_length

             
exception Reading_failure 
let read_channel ch buf offs len = 
  let%lwt read_len = Lwt_unix.read ch buf offs len in
  if read_len < len then
    raise Reading_failure
  else Lwt.return buf
  
        
let decode in_ch  : (message, string) result Lwt.t=
  let%lwt rcv_size = read_channel in_ch rcv_size 0 size_of_length
  in
  let len = Int64.to_int @@ Bytes.get_int64_le rcv_size 0 in
  let _ = Format.printf "Reading %i chars@." len in
  let buf = Bytes.create len in
  let%lwt read = Lwt_unix.read in_ch buf 0 len in
  if read < len then
    raise Reading_failure
  else Yojson.Safe.from_string (Bytes.to_string buf)
       |> message_of_yojson 
       |> Lwt.return
