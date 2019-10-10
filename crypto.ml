open Hacl
type hash = Bigstring.t
type signature = Bigstring.t
type pkh = Bigstring.t

let generate () = Sign.keypair ()

let sign ~sk ~msg =
  let signature =
    Bigstring.create Sign.bytes in
  Sign.sign ~sk ~msg ~signature;
  signature

let verify ~pk ~msg ~signature =
  Sign.verify ~pk ~msg ~signature

let encode_pk pk =
  let buf = Bigstring.create Sign.pkbytes in
  Sign.blit_to_bytes pk buf;
  buf

let encode_sk sk =
  let buf = Bigstring.create Sign.skbytes in
  Sign.blit_to_bytes sk buf;
  buf


type pk = public Sign.key
(* let pk_of_yojson pkj =
 *   Yojson.Safe.
 *             [@encoding encode_pk]
 *             [@@deriving yojson]
 * type sk = secret Sign.key
 *             [@encoding encode_sk]
 *             [@@deriving yojson] *)

              
              

            
