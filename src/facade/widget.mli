open Js_of_ocaml

module Project : sig
  class type boot_input =
    object
      method project :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method container :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Js.meth > Js.t
end
