
type letterbag = {author: Messages.author_id ; sk : Crypto.sk ; mutable content : char list}

let remove bag c =
  bag.content <- Utils.remove_first bag.content c

open Messages
open Crypto
open Utils

let genesis = hash Bigstring.empty

let sign_letter ~sk ~c ~head ~author =
  sign ~sk
    ~msg:(
      hash_to_bigstring @@
      hash_list
        [bigstring_of_char c;
         hash_to_bigstring head;
         pk_to_bigstring author])

let sign_letter_alt ~sk ~c ~head ~author =
  let head = hash_to_bigstring head
  and author = pk_to_bigstring author in
  let head_length = Bigstring.length head in
  let author_length = Bigstring.length author in
  let msg =
    Bigstring.create
      (1+ head_length + author_length)in
  let c = bigstring_of_char c in
  Bigstring.blit c 0 msg 0 1 ;
  Bigstring.blit head 0 msg 1 head_length  ;
  Bigstring.blit author 0 msg (1+head_length) author_length ;
  sign ~sk ~msg


let make_letter_on_block bag c block : letter =
  let head = hash block in
  let signature =
    sign_letter ~sk:bag.sk ~c ~head ~author:bag.author in
  let signature_alt =
    sign_letter_alt ~sk:bag.sk ~c ~head ~author:bag.author in
  assert (signature_alt = signature);
  { letter=c ; head; author=bag.author; signature }

let make_letter_on_hash bag c head_hash : letter=
  let head = head_hash in
  let signature = sign_letter ~sk:bag.sk ~c ~head ~author:bag.author in
  { letter=c ; head; author=bag.author; signature }
