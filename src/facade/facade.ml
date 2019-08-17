open Js_of_ocaml

let start f = Dom_html.window##.onload := f

let () =
  Js.export
    "planet"
    (object%js
       method start f = start f

       val project = Widget.Project.api

       val roe = Roe.api
    end)
;;
