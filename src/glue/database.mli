(** Exposes static databases *)

open Baremetal

(** {2 API} *)

type _ t

val path : _ t -> File.name
(** Fetch the path of a database. *)

(** {2 Static databases} *)

val projects : Shapes.Project.t t
val sectors : Shapes.Sector.t t
val logs : Shapes.Log.t t
val stories : Shapes.Story.t t
val twtxt : Shapes.Twtxt.t t
