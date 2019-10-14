
type letterbag = {author: Messages.author_id ; sk : Crypto.sk ; mutable content : char list}

let remove bag c =
  bag.content <- Utils.remove_first bag.content c

open Messages
open Crypto
open Utils

let genesis = hash Bigstring.empty

let sign_letter ~sk ~c ~head ~period ~author =
  sign ~sk
    ~msg:(
      hash_to_bigstring @@
      hash_list
        [bigstring_of_char c;
         bigstring_of_int period ;
         hash_to_bigstring head;
         pk_to_bigstring author])

let sign_letter_alt ~sk ~c ~head ~period ~author =
  let head = hash_to_bigstring head
  and author = pk_to_bigstring author 
  and period = bigstring_of_int period in
  let head_length = Bigstring.length head in
  let author_length = Bigstring.length author in
  let period_length = Bigstring.length period in
  let msg =
    Bigstring.create
      (1+ period_length+ head_length + author_length)in
  let c = bigstring_of_char c in
  Bigstring.blit c 0 msg 0 1 ;
  Bigstring.blit period 0 msg 1 period_length  ;
  Bigstring.blit head 0 msg (1+period_length) head_length  ;
  Bigstring.blit author 0 msg (1+period_length+head_length) author_length ;
  sign ~sk ~msg


let make_letter_on_block bag period block c : letter =
  let head = hash block in
  let signature =
    sign_letter ~sk:bag.sk ~c ~head ~period ~author:bag.author in
  let signature_alt =
    sign_letter_alt ~sk:bag.sk ~c ~head ~period ~author:bag.author in
  assert (signature_alt = signature);
  { letter=c ; period; head; author=bag.author; signature }

let make_letter_on_hash bag period head_hash c : letter=
  let head = head_hash in
  let signature = sign_letter ~sk:bag.sk ~c ~head ~period ~author:bag.author in
  { letter=c ; period; head; author=bag.author; signature }
