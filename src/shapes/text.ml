open Bedrock
open Paperwork
open Error

module Format = struct
  type t =
    | Org
    | Markdown
    | Raw

  let to_string = function
    | Org ->
      "org"
    | Markdown ->
      "markdown"
    | Raw ->
      "raw"
  ;;

  let from_string str =
    match String.lowercase_ascii str with
    | "raw" ->
      Ok Raw
    | "markdown" | "md" ->
      Ok Markdown
    | "org" | "orgmode" | "org-mode" ->
      Ok Org
    | x ->
      Error (Unknown_format x)
  ;;
end

type content =
  | File of string
  | Plain of string

let patch_flag str x f =
  match String.lowercase_ascii str with
  | "text" | "plain" | "internal" | "intern" ->
    Ok (f, Plain x)
  | "file" | "extern" | "external" ->
    Ok (f, File x)
  | _ ->
    Error [Invalid_text_scheme]
;;

type t = Format.t * content

let fetch table field =
  match Hashtbl.find_opt table field with
  | None ->
    Error [Undefined_field field]
  | Some (Some Qexp.(Node children)) ->
    (match children with
    | Qexp.([ (String (_, flag) | Keyword flag | Tag flag | Atom flag)
            ; (String (_, fmt) | Keyword fmt | Tag fmt | Atom fmt)
            ; String (_, content) ]) ->
      let open Validation.Infix in
      Format.from_string fmt |> Validation.from_result
      >>= patch_flag flag content
    | _ ->
      Error [Invalid_field field])
  | _ ->
    Error [Invalid_field field]
;;
