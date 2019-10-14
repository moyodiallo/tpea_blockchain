open Messages
   
type mempool

val create : unit -> mempool
val gen_letters : politician_id -> char list
val register : mempool -> politician_id -> unit

val letterpool : mempool -> letterpool
val letterpool_since : mempool -> period -> letterpool

val wordpool_since : mempool -> period -> wordpool
val wordpool : mempool -> wordpool

val inject_letter : ?turn_by_turn:bool -> mempool -> letter -> (period option*bool)
val inject_word : ?turn_by_turn:bool -> mempool -> word -> (period option*bool)
