open Bedrock
open Util

type metadata_key = string
type metadata = string

let render key f value = Format.asprintf "%s: %s\n" key (f value)
let render_string key value = render key id value

let may_render_with key f default = function
  | None ->
    render_string key default
  | Some value ->
    render key f value
;;

let may_render key f = function None -> "" | Some value -> render key f value

let may_render_with_format ~default f key value =
  may_render_with key (fun x -> Format.asprintf "%a" f x) default value
;;

let render_if key flag = if flag then render_string key "true" else ""
let join rules = "---\n" ^ String.concat "" rules ^ "---\n"
