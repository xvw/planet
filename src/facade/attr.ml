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

module Data = struct
  let ( .%{} ) element attribute = element.%{("data-" ^ attribute)}

  let ( .%{}<- ) element attribute value =
    element.%{("data-" ^ attribute)} <- value
  ;;
end
