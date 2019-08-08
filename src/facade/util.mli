open Js_of_ocaml

module Array : sig
  val empty : unit -> 'a Js.js_array Js.t
  val from_list : ('a -> 'b) -> 'a list -> 'b Js.js_array Js.t
end

module Promise : sig
  val run : ('a -> 'b Lwt.t) -> ('b -> 'c) -> 'a -> 'c Lwt.t
  val dom_onload : unit -> unit Lwt.t
end
