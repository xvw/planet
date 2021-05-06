open Js_of_ocaml

type t = Dom_html.storage Js.t

exception Not_supported
exception Not_found

class type storageEvent =
  object
    inherit Dom_html.event
    method key : Js.js_string Js.t Js.opt Js.readonly_prop
    method oldValue : Js.js_string Js.t Js.opt Js.readonly_prop
    method newValue : Js.js_string Js.t Js.opt Js.readonly_prop
    method url : Js.js_string Js.t Js.readonly_prop
    method storageArea : Dom_html.storage Js.t Js.opt Js.readonly_prop
  end

type event = storageEvent Js.t

val event : event Dom.Event.typ

module type STORAGE = sig
  type key = string
  type value = string
  type old_value = string
  type url = string

  type change_state =
    | Clear
    | Insert of key * value
    | Remove of key * old_value
    | Update of key * old_value * value

  val dump_change_state : change_state -> string
  val is_supported : unit -> bool
  val handler : t
  val length : unit -> int
  val get : key -> value option
  val set : key -> value -> unit
  val remove : key -> unit
  val clear : unit -> unit
  val key : int -> key option
  val at : int -> (key * value) option
  val to_hashtbl : unit -> (key, value) Hashtbl.t
  val iter : (key -> value -> unit) -> unit
  val find : (key -> value -> bool) -> (key * value) option
  val select : (key -> value -> bool) -> (key, value) Hashtbl.t

  val on_change
    :  ?prefix:string
    -> (change_state -> url -> unit)
    -> Dom.event_listener_id

  val on_clear : (url -> unit) -> Dom.event_listener_id

  val on_insert
    :  ?prefix:string
    -> (key -> value -> url -> unit)
    -> Dom.event_listener_id

  val on_remove
    :  ?prefix:string
    -> (key -> old_value -> url -> unit)
    -> Dom.event_listener_id

  val on_update
    :  ?prefix:string
    -> (key -> old_value -> value -> url -> unit)
    -> Dom.event_listener_id
end

module Local : STORAGE
module Session : STORAGE
