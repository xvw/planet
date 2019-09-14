open Js_of_ocaml
open Bedrock

module Log : sig
  class type js =
    object
      method uuid : Js.js_string Js.t Js.readonly_prop

      method date : Js.js_string Js.t Js.readonly_prop

      method duration : int Js.readonly_prop

      method sector : Js.js_string Js.t Js.readonly_prop

      method project : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

      method label : Js.js_string Js.t Js.readonly_prop
    end

  type t = js Js.t

  val shape : t -> Shapes.Log.t Validation.t
  val hydrate : unit -> unit Lwt.t
  val get_by_id : string -> Shapes.Log.t option
end

module Location : sig
  class type js =
    object
      method date : Js.js_string Js.t Js.readonly_prop

      method country : Js.js_string Js.t Js.readonly_prop

      method city : Js.js_string Js.t Js.readonly_prop
    end

  type t = js Js.t

  val shape
    :  t
    -> (Paperwork.Timetable.Day.t * string * string) Validation.t

  val get
    :  unit
    -> (Paperwork.Timetable.Day.t * string * string) list
       Validation.t
       Lwt.t
end
