open Js_of_ocaml

module Project : sig
  val api
    : < boot :
          Dom_html.textAreaElement Js.t Js.Opt.t
          -> Dom_html.element Js.t Js.Opt.t
          -> unit Js.meth >
      Js.t
end
