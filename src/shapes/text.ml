open Bedrock
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

type t = Format.t * content
