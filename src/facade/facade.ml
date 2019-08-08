open Js_of_ocaml

let hello nickname =
  Console.print ("hello " ^ Js.to_string nickname ^ " from planet !")
;;

let () =
  Js.export
    "planet"
    (object%js
       method hello nickname = hello nickname

       val project = Widget.Project.api
    end)
;;
