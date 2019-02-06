(** Terminal related UI. *)

open Bedrock
open Baremetal

(** {2 Prompt errors} *)

val prompt_errors : ?intro:bool -> Error.t list -> unit
val prompt_error : ?intro:bool -> Error.t -> unit

(** {2 Boxes and Tables} *)

val link_box :
     ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?f:(Shapes.Link.simple -> Ansi.fragments)
  -> string
  -> Shapes.Link.simple list
  -> Ansi.fragments

val dated_link_box :
     ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?f:(Shapes.Link.dated -> Ansi.fragments)
  -> string
  -> Shapes.Link.dated list
  -> Ansi.fragments
