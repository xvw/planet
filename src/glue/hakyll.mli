(** Hakyll helper *)

type metadata_key = string
type metadata = string

(** {2 Render helpers} *)

val render : metadata_key -> ('a -> string) -> 'a -> metadata
val render_string : metadata_key -> string -> metadata

val may_render
  :  metadata_key
  -> ('a -> string)
  -> 'a option
  -> metadata

val may_render_with
  :  metadata_key
  -> ('a -> string)
  -> string
  -> 'a option
  -> metadata

val may_render_with_format
  :  default:string
  -> (Format.formatter -> 'a -> unit)
  -> metadata_key
  -> 'a option
  -> metadata

val render_if : metadata_key -> bool -> metadata
val join : metadata list -> metadata
