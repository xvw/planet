(** Exposes static databases *)

open Shapes
open Baremetal

(** {2 API} *)

type _ t

(** Fetch the path of a database. *)
val path : _ t -> File.name

(** Read a database *)
val read : (File.name -> 'a) -> 'a t -> 'a

(** {2 Static databases} *)

val projects : Project.t list t
