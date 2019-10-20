open Messages
   
type mempool

val create : turn_by_turn:bool -> ?nb_rounds:int -> ?timeout:float -> unit -> mempool

val turn_by_turn : mempool -> bool
  
val gen_letters : mempool -> politician_id -> char list
val register : mempool -> politician_id -> unit

val letterpool : mempool -> letterpool
val letterpool_since : mempool -> period -> letterpool

val wordpool_since : mempool -> period -> wordpool
val wordpool : mempool -> wordpool

val inject_letter : mempool -> letter -> (period option*bool)
val inject_word : mempool -> word -> (period option*bool)
