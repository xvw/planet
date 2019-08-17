(** Describe meta data as HTML nodes *)

type t = (string * string) list
type html = string

val to_html : string list -> t -> html
