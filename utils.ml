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
  | h::t -> h:: aux t d (h::res)
  in aux l d []

let bigstring_of_char c =
  Bigstring.of_string (Format.sprintf "%c" c)
