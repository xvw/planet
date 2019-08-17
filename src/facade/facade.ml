open Js_of_ocaml

let hello nickname =
  Console.print ("hello " ^ Js.to_string nickname ^ " from planet !")
;;

let start f = Dom_html.window##.onload := f

let () =
  Js.export
    "planet"
    (object%js
       method hello nickname = hello nickname

       method start f = start f

       val project = Widget.Project.api
    end)
;;
