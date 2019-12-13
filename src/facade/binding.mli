open Js_of_ocaml
open Bedrock

module Project : sig
  class type short_js =
    object
      method name : Js.js_string Js.t Js.readonly_prop

      method published : bool Js.t Js.Optdef.t Js.readonly_prop
    end

  type short = short_js Js.t

  val short_shape : short -> string * bool
  val get : unit -> (string, bool) Hashtbl.t Lwt.t
end

module Log : sig
  class type js =
    object
      method uuid : Js.js_string Js.t Js.readonly_prop

      method date : Js.js_string Js.t Js.readonly_prop

      method duration : int Js.readonly_prop

      method sector : Js.js_string Js.t Js.readonly_prop

      method project : Js.js_string Js.t Js.Opt.t Js.readonly_prop

      method label : Js.js_string Js.t Js.readonly_prop
    end

  type t = js Js.t

  val shape : t -> Shapes.Log.t Validation.t
  val hydrate : unit -> unit Lwt.t
  val get_by_id : string -> Shapes.Log.t option
  val get_last_logs : unit -> Shapes.Log.t list Lwt.t
  val collect : unit -> (string, Shapes.Log.t list) Hashtbl.t Lwt.t
end

module Location : sig
  class type js =
    object
      method date : Js.js_string Js.t Js.readonly_prop

      method country : Js.js_string Js.t Js.readonly_prop

      method city : Js.js_string Js.t Js.readonly_prop
    end

  type t = js Js.t

  val shape : t -> (Paperwork.Timetable.Day.t * string * string) Validation.t

  val get :
       unit
    -> (Paperwork.Timetable.Day.t * string * string) list Validation.t Lwt.t
end
