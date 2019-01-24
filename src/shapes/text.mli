(** Describe a text with a potential preprocessor. *)

open Bedrock

(** {2 Format} *)

module Format : sig
  type t =
    | Org
    | Markdown
    | Raw

  val to_string : t -> string
  val from_string : string -> t Result.t
end

type content =
  | File of string
  | Plain of string

type t = Format.t * content
