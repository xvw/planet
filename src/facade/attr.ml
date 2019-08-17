open Js_of_ocaml
open Bedrock.Util

let ( .%{} ) element attribute =
  let key = Js.string attribute in
  let attr = element##getAttribute key in
  Js.Opt.to_option attr |> Option.map Js.to_string
;;

let ( .%{}<- ) element attribute value =
  let attr = Js.string attribute in
  let svalue = Js.string value in
  element##setAttribute attr svalue
;;

let ( .?{} ) element attribute =
  let key = Js.string attribute in
  let exists = element##hasAttribute key in
  Js.to_bool exists
;;

module Data = struct
  let ( .%{} ) element attribute = element.%{("data-" ^ attribute)}

  let ( .%{}<- ) element attribute value =
    element.%{("data-" ^ attribute)} <- value
  ;;

  let ( .?{} ) element attribute = element.?{("data-" ^ attribute)}
end
