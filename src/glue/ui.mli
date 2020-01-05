(** Terminal related UI. *)

open Baremetal

(** {2 Lock} *)

val ensure_sectors_projects
  :  ((string, Shapes.Sector.t) Hashtbl.t
      -> Shapes.Context.Projects.t
         * (Shapes.Project.t
           * Paperwork.Timetable.Day.t option
           * Shapes.Context.Projects.context option)
           list
      -> unit)
  -> unit

(** {2 Boxes and Tables} *)

val link_box
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?f:(Shapes.Link.simple -> Ansi.fragments)
  -> string
  -> Shapes.Link.simple list
  -> Ansi.fragments

val dated_link_box
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?f:(Shapes.Link.dated -> Ansi.fragments)
  -> string
  -> Shapes.Link.dated list
  -> Ansi.fragments

(** {2 Prompters} *)

val may_project : Shapes.Project.t list -> Shapes.Project.t option
val select_sectors : (string, Shapes.Sector.t) Hashtbl.t -> string list
val get_string : string -> string -> string
val get_string_opt : string -> string -> string option
val get_day_opt : string -> string -> Paperwork.Timetable.Day.t option
