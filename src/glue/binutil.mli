(** {2 Binaries helpers} *)

val ensure_sectors_projects :
     (   (string, Shapes.Sector.t) Hashtbl.t
      -> Shapes.Context.Projects.t
         * ( Shapes.Project.t
           * Paperwork.Timetable.Day.t option
           * Shapes.Context.Projects.context option
           )
           list
      -> unit)
  -> unit

val may_project : Shapes.Project.t list -> Shapes.Project.t option
val select_sectors : (string, Shapes.Sector.t) Hashtbl.t -> string list
val get_string : string -> string -> string
val get_string_opt : string -> string -> string option
val get_day_opt : string -> string -> Paperwork.Timetable.Day.t option
