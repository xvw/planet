(** Terminal related UI. *)

open Baremetal

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
