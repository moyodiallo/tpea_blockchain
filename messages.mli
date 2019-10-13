open Crypto

type period = int [@@deriving yojson,show]

type author_id = pk  [@@deriving yojson,show]

type politician_id = pk  [@@deriving yojson,show]


type letter =
  { letter : char ;
    head : hash ;
    author : author_id ;
    signature : signature
  }
    [@@deriving yojson,show]

type word = {
    word : letter list ;
    head : hash ;
    politician : politician_id ;
    signature : signature
  } [@@deriving yojson,show]



type letterpool =
  {mutable period : int ; mutable letters : (int*letter) list } [@@deriving
                                                     yojson,show]
val add_letter : letterpool -> period -> letter -> unit
val remove_letter : letterpool -> letter -> unit
val next_period_letter : letterpool -> unit
val find_by_author :
  letterpool ->
  ?period:(period->bool) ->
  author_id ->
  (int*letter) list

type wordpool =
  {mutable period : int ; mutable words : (int*word) list } [@@deriving
                                                     yojson,show]
val add_word : wordpool -> period -> word -> unit
val next_period_word : wordpool -> unit

val init_letterpool : letterpool
val init_wordpool : wordpool

type diff_letterpool_arg = {since : period; letterpool : letterpool}[@@deriving yojson,show]
type diff_wordpool_arg = {since : period; wordpool : wordpool}[@@deriving yojson,show]

type message =
  | Register of author_id
  | Listen
  | Stop_listen
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
