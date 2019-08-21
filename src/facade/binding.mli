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
end
