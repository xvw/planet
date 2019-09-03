(** Build complexe QExpression with Key Value *)

open Paperwork

val option : 'a option -> string -> ('a -> string) -> Qexp.t list
val content : Text.t option -> Qexp.t list
val list : string -> 'a list -> ('a -> Qexp.t) -> Qexp.t list

val ziplist
  :  string
  -> (string * 'a list) list
  -> ('a -> Qexp.t)
  -> Qexp.t list
