(** Deal with Html Attributes *)

open Js_of_ocaml

val ( .%{} ) : #Dom.element Js.t -> string -> string option
val ( .%{}<- ) : #Dom.element Js.t -> string -> string -> unit

module Data : sig
  val ( .%{} ) : #Dom.element Js.t -> string -> string option
  val ( .%{}<- ) : #Dom.element Js.t -> string -> string -> unit
end
