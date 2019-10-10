type hash = bytes [@@deriving yojson]
type signature = bytes [@@deriving yojson]
type pkh = hash [@@deriving yojson]
(* type pk = bytes  [@@deriving yojson] *)

type pk = Bigstring.t 
            [@to_yojson Hex.of_bigstring]
            [@of_yojson Hex.to_bigstring]               
            [@@deriving yojson]
        
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
  | Inject_raw_op of bytes[@@deriving yojson]

val decode : Lwt_unix.file_descr -> (message, string) result Lwt.t

val encode : message -> Lwt_unix.file_descr -> unit Lwt.t

(* let encoding msg_encoding =
 *   let open Data_encoding in
 *   dynamic_size @@
 *   union ~tag_size:`Uint16
 *     ([ case (Tag 0x01) ~title:"register"
 *          (obj1 (req "id" author_id_encoding))
 *          (function Register author_id -> Some author_id | _ -> None)
 *          (fun author_id -> Register author_id);
 *        case (Tag 0x02) ~title:"Full mempool"
 *          (obj1 (req "mempool" mempool_encoding))
 *          (function Full_mempool mempool -> Some mempool | _ -> None)
 *          (fun mempool -> Full_mempool mempool); *)
         
