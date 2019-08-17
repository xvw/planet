(** Deal with Html Attributes *)

open Js_of_ocaml

val ( .%{} ) : #Dom.element Js.t -> string -> string option
val ( .%{}<- ) : #Dom.element Js.t -> string -> string -> unit
val ( .?{} ) : #Dom.element Js.t -> string -> bool

module Data : sig
  val ( .%{} ) : #Dom.element Js.t -> string -> string option
  val ( .%{}<- ) : #Dom.element Js.t -> string -> string -> unit
  val ( .?{} ) : #Dom.element Js.t -> string -> bool
end
