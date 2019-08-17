open Js_of_ocaml

module Sector : sig
  val nodelist_to_hashtbl
    :  Dom_html.element Dom.nodeList Js.t
    -> (string, Shapes.Sector.t) Hashtbl.t Bedrock.Validation.t
end

module Project : sig
  class type boot_input =
    object
      method timedata :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method project :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method rightContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method bottomContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method sectors :
        Dom_html.element Dom.nodeList Js.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Js.meth > Js.t
end
