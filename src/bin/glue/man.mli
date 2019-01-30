(** Default Manpage *)
val default :
     ?other_authors:(string * string * string option) list
  -> string
  -> Cmdliner.Manpage.block list
