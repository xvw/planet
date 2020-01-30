open Js_of_ocaml

module Common : sig
  val api
    : < timeAgo : Dom_html.element Dom.nodeList Js.t -> unit Js.meth > Js.t
end

module Sector : sig
  val nodelist_to_hashtbl
    :  Dom_html.element Dom.nodeList Js.t
    -> (string, Shapes.Sector.t) Hashtbl.t Bedrock.Validation.t
end

module Project : sig
  class type boot_input =
    object
      method timedata : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method project : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method rightContainer : Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method bottomContainer : Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method sectors : Dom_html.element Dom.nodeList Js.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Lwt.t Js.meth > Js.t
end

module Story : sig
  class type boot_input =
    object
      method path : Js.js_string Js.t Js.readonly_prop

      method eof : Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method story : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method rightContainer : Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method bottomContainer : Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method resumeDetails : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Lwt.t Js.meth > Js.t
end

module Location : sig
  class type boot_input =
    object
      method locationBox : Dom_html.element Js.t Js.Opt.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Lwt.t Js.meth > Js.t
end

module Diary : sig
  class type boot_input =
    object
      method context : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method calendarBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop

      method titleBox : Dom_html.headingElement Js.t Js.Opt.t Js.readonly_prop

      method statisticBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop

      method entryBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop

      method sectors : Dom_html.element Dom.nodeList Js.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Lwt.t Js.meth > Js.t
end

module Tags : sig
  class type boot_input =
    object
      method tagsBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop

      method contentBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  class type random_input =
    object
      method button : Dom_html.buttonElement Js.t Js.Opt.t Js.readonly_prop
    end

  val api
    : < boot : boot_input Js.t -> unit Lwt.t Js.meth
      ; random : random_input Js.t -> unit Lwt.t Js.meth >
      Js.t
end

module Tasks : sig
  class type boot_input =
    object
      method boardBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Lwt.t Js.meth > Js.t
end

module Gallery : sig
  class type boot_input =
    object
      method gallery : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method container : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop

      method rightContainer : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  val api : < boot : boot_input Js.t -> unit Lwt.t Js.meth > Js.t
end
