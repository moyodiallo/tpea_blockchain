open Error

let hex_of_hexstring str : Hex.t=
  `Hex str


type hash = bytes [@@deriving yojson]

let pp_hash ppf hash =
  Hex.of_bytes hash |> Hex.pp ppf
let show_hash hash =
  Format.asprintf "%a" pp_hash hash

type signature = bytes [@@deriving yojson]

let pp_signature = pp_hash
let show_signature signature =
  Format.asprintf "%a" pp_signature signature

type pkh = hash [@@deriving yojson,show]
(* type pk = bytes  [@@deriving yojson] *)

type pk = Bigstring.t
let pk_to_yojson pk =
    [%to_yojson: string]
    @@ (Format.asprintf "%a" Hex.pp)
    @@ Hex.of_bigstring  pk



let pk_of_yojson pkj =
   ( [%of_yojson: string] pkj) >>?
     hex_of_hexstring  >>?
     Hex.to_bigstring

let pp_pk ppf pk =
  Hex.pp ppf  @@ Hex.of_bigstring pk
let show_pk pk =
  Format.asprintf "%a" pp_pk pk

type period = int [@@deriving yojson,show]

type author_id = pk  [@@deriving yojson,show]

type politician_id = pk  [@@deriving yojson,show]


type letter =
  { letter : char ;
    head : hash ;
    id : author_id ;
    signature : signature
  }
    [@@deriving yojson,show]

type word = {
    word : letter list ;
    head : hash ;
    id : politician_id ;
    signature : signature
  } [@@deriving yojson,show]

type mempool =
  {period : int ; letters : letter list } [@@deriving yojson,show]

type diff_mempool_arg = {since : period; mempool : mempool}[@@deriving yojson,show]

type message =
  | Register of author_id
  | Full_mempool of mempool
  | Diff_mempool of diff_mempool_arg
  | Get_full_mempool
  | Get_mempool_since of period
  | Inject_letter of letter
  | Inject_word of word
  | Inject_raw_op of bytes
                       [@@deriving show]

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


let send msg out_ch =
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
    let _ = Format.printf "Reading failed:  read %i instead of %i
                           chars : @ %a@."
              read_len
              len
              Hex.pp (Hex.of_bytes buf)
    in
    raise Reading_failure
  else Lwt.return buf


let receive in_ch  : (message, string) result Lwt.t=
  let%lwt () = Format.printf "receiving Message.@."; Lwt.return_unit in
  let%lwt rcv_size = read_channel in_ch rcv_size 0 size_of_length in
  let%lwt () = Format.printf "reading size.@."; Lwt.return_unit in
  let len = Int64.to_int @@ Bytes.get_int64_le rcv_size 0 in
  let _ = Format.printf "Reading %i chars@." len in
  let buf = Bytes.create len in
  let%lwt read = Lwt_unix.read in_ch buf 0 len in
  if read < len then
    raise Reading_failure
  else
    let _ = Format.printf "All data read, processing %i chars@." len in
    Yojson.Safe.from_string (Bytes.to_string buf)
       |> message_of_yojson
       |> Lwt.return
