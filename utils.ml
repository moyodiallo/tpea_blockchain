open Error
   
(* utilities  *)
let hex_of_hexstring str : Hex.t=
  `Hex str

let bigstring_to_yojson (bs:Bigstring.t) : Yojson.Safe.t=
    [%to_yojson: string]
    @@ (Format.asprintf "%a" Hex.pp)
    @@ Hex.of_bigstring  bs

let bigstring_of_yojson (bsj:Yojson.Safe.t) =
  ( [%of_yojson: string] bsj) >>?
     hex_of_hexstring  >>?
     Hex.to_bigstring

let pp_bs ppf bs =
  Hex.of_bigstring bs |> Hex.pp ppf

let show_bs bs =
  Format.asprintf "%a" pp_bs bs

let unopt_map f ~default opt =
  Option.value ~default (Option.map f opt)

let is_nil l =  l = []

(* let rec remove_first l d =
 *   match l with
 *     [] -> []
 *   | h::t when h=d -> t
 *   | h::t -> h::(remove_first t d) *)

let remove_first l d =
  let rec aux l d res =
  match l with
    [] -> List.rev res
  | h::t when h=d -> List.rev_append res t
  | h::t -> aux t d (h::res)
  in aux l d []

let bigstring_of_char c =
  Bigstring.of_string (Format.sprintf "%c" c)

let bytes_of_int i =
  let ibuf = Bytes.create 8 in
  Bytes.set_int64_be ibuf 0 (Int64.of_int i);
  ibuf

let bigstring_of_int i =
  let bs = Bigstring.create 8 
  in Bigstring.fill bs '\000';
     let ibuf = bytes_of_int i in
     Bigstring.blit_of_bytes ibuf 0 bs 0 8 ;
     bs
     
let included small large =
  List.fold_left
    (fun i s -> if i then List.mem s large else i)
    true small
  
(* tests *)
let test_remove_first () =
  let l = [1;2;3;4;5;6] in
  assert (remove_first l 1 = [2;3;4;5;6]);
  assert (remove_first l 2 = [1;3;4;5;6]);
  assert (remove_first l 3 = [1;2;4;5;6]);
  assert (remove_first l 6 = [1;2;3;4;5]);
  assert (remove_first l 0 = [1;2;3;4;5;6])

let test_included () =
  let l = [1;2;3;4;5;6;4;5;6] in
  assert (included [1;2] l );
  assert (included [2;3;4] l );
  assert (included [2;3;4] l );
  assert (included [2;3;4;4] l );
  assert (included [2] l );
  assert (not @@ included [0;2;3;4;4;9] l );
  assert (not @@ included [0;2;3;4;4;0;9] l );
  assert (not @@ included [0;2;3;4] l );
  assert (not @@ included [2;3;4;0] l );
  assert (not @@ included [0] l )
