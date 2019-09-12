open Js_of_ocaml

val window : Dom_html.window Js.t
val document : Dom_html.document Js.t
val scroll_y : unit -> int
val scroll_x : unit -> int
val get_by_id : string -> Dom_html.element Js.t option
val clear : #Dom_html.element Js.t -> unit
val offset_y : #Dom_html.element Js.t -> int
val watch_once : ('a -> 'b Lwt.t) -> 'a -> ('b -> unit) -> unit Lwt.t
val watch : ('a -> 'b Lwt.t) -> 'a -> ('b -> unit) -> unit Lwt.t
