(** Exposes static databases *)

open Baremetal

(** {2 API} *)

type _ t

(** Fetch the path of a database. *)
val path : _ t -> File.name

(** {2 Static databases} *)

val projects : Shapes.Project.t t
val sectors : Shapes.Sector.t t
val logs : Shapes.Log.t t
