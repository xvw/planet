(** Describe a text with a potential preprocessor. *)

open Bedrock
open Paperwork

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

(** {2 Helpers} *)

val fetch : (t, 'a) Table.Fetch.t
val fetch_opt : (t option, 'a) Table.Fetch.t
