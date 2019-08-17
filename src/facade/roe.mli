(** Rich Org Extension
    
    Extend Org format using JavaScript
*)

open Js_of_ocaml

val api : < mount : Dom_html.element Js.t -> unit Js.meth > Js.t
