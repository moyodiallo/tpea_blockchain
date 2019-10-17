open Crypto

type period = int [@@deriving yojson,show]

type author_id = pk  [@@deriving yojson,show]

type politician_id = pk  [@@deriving yojson,show]


type letter =
  { letter : char ;
    period : period ;
    head : hash ;
    author : author_id ;
    signature : signature
  }
    [@@deriving yojson,show]

val  letter_to_bigstring : letter -> Bigstring.t
  
type word = {
    word : letter list ;
    head : hash ;
    politician : politician_id ;
    signature : signature
  } [@@deriving yojson,show]



type letterpool =
  {mutable current_period : period ;
   mutable next_period : period ;
   mutable letters : (int*letter) list } [@@deriving yojson,show]

type wordpool =
  {mutable current_period : period ;
   mutable next_period : period ;
   mutable words : (int*word) list } [@@deriving yojson,show]

type diff_letterpool_arg = {since : period; letterpool : letterpool}[@@deriving yojson,show]
type diff_wordpool_arg = {since : period; wordpool : wordpool}[@@deriving yojson,show]

type message =
  | Register of author_id
  | Listen
  | Stop_listen
  | Next_turn of period
  | Letters_bag of char list
  | Full_letterpool of letterpool
  | Full_wordpool of wordpool
(*   | New_letter of letter *)
(*   | New_word of word *)
  | Diff_letterpool of diff_letterpool_arg
  | Diff_wordpool of diff_wordpool_arg
  | Get_full_letterpool
  | Get_full_wordpool
  | Get_letterpool_since of period
  | Get_wordpool_since of period
  | Inject_letter of letter
  | Inject_word of word
  | Inject_raw_op of bytes
                       [@@deriving yojson,show]

val receive : ?verbose:bool -> Lwt_unix.file_descr -> (message, string) result Lwt.t

val send : ?verbose:bool -> message -> Lwt_unix.file_descr -> unit Lwt.t
